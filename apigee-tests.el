;;; apigee-tests.el --- ERT tests for apigee.el  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run from this directory with:
;;
;;   emacs -Q --batch -L . -f package-initialize \
;;     -l apigee-tests.el -f ert-run-tests-batch-and-exit
;;
;; `package-initialize' provides yasnippet.
;;
;; Loading apigee.el reads, and on exit writes, the settings file in
;; `user-emacs-directory'. So this file points that at a temporary
;; directory before requiring apigee, to keep the real settings
;; untouched.
;;
;; Test names carry the number of the item in the 2026-10-26 code review
;; that they guard, where there is one.

;;; Code:

(require 'ert)
(require 'lisp-mnt)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst apigee-tests--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory holding apigee.el and this file.")

(defconst apigee-tests--home (file-name-as-directory (make-temp-file "apigee-tests-home" t))
  "Throwaway `user-emacs-directory' for the test session.")

(setq user-emacs-directory apigee-tests--home)
;; Appended, so it runs after apigee's own persist-on-exit hook.
(add-hook 'kill-emacs-hook (lambda () (delete-directory apigee-tests--home t)) t)

(require 'apigee)
(require 'xml)
(apigee-setup)

;;; Fixtures

(defmacro apigee-tests--with-temp-dir (var &rest body)
  "Bind VAR to a fresh temporary directory (a truename) while running BODY.
Afterwards, kill buffers visiting files under it and delete it."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (file-truename (make-temp-file "apigee-test" t)))))
     (unwind-protect
         (progn ,@body)
       (dolist (buf (buffer-list))
         (let ((dir (with-current-buffer buf
                      (or buffer-file-name
                          (and (derived-mode-p 'dired-mode) default-directory)))))
           (when (and dir (string-prefix-p ,var (file-truename dir)))
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf))))
       (delete-directory ,var t))))

(defun apigee-tests--new-asset (asset-type name template-name dir)
  "Create asset NAME of ASSET-TYPE in DIR from the repo template TEMPLATE-NAME.
Return the bundle directory, e.g. DIR/NAME/apiproxy/."
  (let ((alist (symbol-value (apigee--asset-kind asset-type :templates))))
    (apigee--new-asset-from-template asset-type name alist template-name dir)
    (file-name-as-directory
     (file-name-concat dir name (apigee--asset-kind asset-type :bundle)))))

(defun apigee-tests--write (file contents)
  "Write CONTENTS to FILE, creating parent directories."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert contents)))

(defun apigee-tests--read (file)
  "Return the contents of FILE, without surrounding whitespace.
Saving may add a final newline, depending on `require-final-newline'."
  (with-temp-buffer (insert-file-contents file) (string-trim (buffer-string))))

(defun apigee-tests--count (regexp string)
  "Return the number of matches of REGEXP in STRING."
  (let ((count 0) (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count) start (match-end 0)))
    count))

(defmacro apigee-tests--with-answers (answers &rest body)
  "Run BODY with `completing-read' and `read-string' returning ANSWERS in turn.
Each prompt is recorded, with its REQUIRE-MATCH argument, in the variable
`apigee-tests--prompts', newest first."
  (declare (indent 1))
  `(let ((remaining ,answers))
     (setq apigee-tests--prompts nil)
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt _coll &optional _pred require-match &rest _)
                  (push (list prompt require-match) apigee-tests--prompts)
                  (pop remaining)))
               ((symbol-function 'read-string)
                (lambda (prompt &rest _)
                  (push (list prompt nil) apigee-tests--prompts)
                  (pop remaining))))
       ,@body)))

(defvar apigee-tests--prompts nil
  "Prompts seen by `apigee-tests--with-answers', newest first.")

(defmacro apigee-tests--with-snippets (&rest body)
  "Run BODY with `apigee--expand-snippet' inserting the snippet text as-is.
The real one waits in a recursive edit for the user to fill in fields."
  `(cl-letf (((symbol-function 'apigee--expand-snippet)
              (lambda (content &rest _) (insert content))))
     ,@body))

(defun apigee-tests--endpoint-bundle (dir endpoint-xml)
  "Create a minimal proxy in DIR whose default.xml endpoint is ENDPOINT-XML.
Return the apiproxy directory."
  (let ((bundle (file-name-as-directory (file-name-concat dir "px" "apiproxy"))))
    (apigee-tests--write (file-name-concat bundle "px.xml") "<APIProxy name=\"px\"/>")
    (apigee-tests--write (file-name-concat bundle "proxies" "default.xml") endpoint-xml)
    (make-directory (file-name-concat bundle "policies") t)
    bundle))

;;; XML handling (0.1, 2.1b)

(ert-deftest apigee-test-0.1-2.1-xml-parse-keeps-comments ()
  "apigee's parser keeps comments, and handles text such as /x (0.1)."
  (let ((root (apigee--parse-xml "<a>\n  <!-- c -->\n  <b>/x</b>\n</a>")))
    (should (equal (car (xml-get-children (car root) 'b)) '(b nil "/x")))
    (should (member '(nil . "<!-- c -->") (xml-node-children (car root))))))

(ert-deftest apigee-test-2.1-xml-override-is-scoped ()
  "The comment-preserving parser and printer apply only inside apigee (2.1b)."
  (apigee--parse-xml "<a/>")
  (should-not (eq (symbol-function 'xml-parse-tag-1) #'apigee--xml-parse-tag-1))
  (should-not (eq (symbol-function 'xml-debug-print-internal) #'apigee--xml-print-internal))
  (let ((root (with-temp-buffer
                (insert "<a><!-- c --><b/></a>")
                (xml-parse-region (point-min) (point-max)))))
    (should-not (assq nil (xml-node-children (car root))))))

(ert-deftest apigee-test-2.1-serialize-keeps-comments ()
  "Parsing and re-serializing an XML file keeps its comments."
  (with-temp-buffer
    (insert "<ProxyEndpoint name=\"default\">\n  <!-- keep me -->\n  <Flows/>\n</ProxyEndpoint>\n")
    (let ((root (apigee--parse-buffer)))
      (apigee--serialize-xml-elt root))
    (should (string-match-p "<!-- keep me -->" (buffer-string)))
    (should (string-match-p "<Flows/>" (buffer-string)))))

;;; Rename policy (1.1, 1.2)

(defun apigee-tests--rename-fixture (dir)
  "Create a bundle in DIR for rename tests; return the apiproxy directory."
  (let ((b (file-name-as-directory (file-name-concat dir "myproxy" "apiproxy"))))
    (apigee-tests--write (file-name-concat b "myproxy.xml") "<APIProxy name=\"myproxy\"/>")
    (apigee-tests--write (file-name-concat b "policies" "AM-Old.xml") "<AssignMessage name=\"AM-Old\"/>")
    (apigee-tests--write (file-name-concat b "policies" "AM-Old-1.xml") "<AssignMessage name=\"AM-Old-1\"/>")
    (apigee-tests--write (file-name-concat b "proxies" "default.xml")
                         (concat "<P><Step><Name>AM-Old</Name></Step><Step><Name>AM-Old-1</Name></Step>"
                                 "<Step><Name>X-AM-Old</Name></Step>"
                                 "<Condition>assignmessage.AM-Old.done = true</Condition>"
                                 "<!-- am-old --></P>"))
    (apigee-tests--write (file-name-concat b "targets" "default.xml")
                         "<T><Step><Name>AM-Old</Name></Step><Step><Name>AM-Old-1</Name></Step></T>")
    b))

(ert-deftest apigee-test-1.1-1.2-rename-policy ()
  "Renaming updates whole references only, and finishes the job."
  (apigee-tests--with-temp-dir dir
    (let* ((b (apigee-tests--rename-fixture dir))
           (default-directory (file-name-concat b "policies/")))
      (apigee-tests--with-answers '("AM-Old" "AM-New")
        (apigee-rename-policy))
      (should (equal (directory-files (file-name-concat b "policies") nil "\\.xml\\'")
                     '("AM-New.xml" "AM-Old-1.xml")))
      (should (equal (apigee-tests--read (file-name-concat b "policies" "AM-New.xml"))
                     "<AssignMessage name=\"AM-New\"/>"))
      (should (equal (apigee-tests--read (file-name-concat b "policies" "AM-Old-1.xml"))
                     "<AssignMessage name=\"AM-Old-1\"/>"))
      (should (equal (apigee-tests--read (file-name-concat b "proxies" "default.xml"))
                     (concat "<P><Step><Name>AM-New</Name></Step><Step><Name>AM-Old-1</Name></Step>"
                             "<Step><Name>X-AM-Old</Name></Step>"
                             "<Condition>assignmessage.AM-New.done = true</Condition>"
                             "<!-- am-old --></P>")))
      (should (equal (apigee-tests--read (file-name-concat b "targets" "default.xml"))
                     "<T><Step><Name>AM-New</Name></Step><Step><Name>AM-Old-1</Name></Step></T>")))))

(ert-deftest apigee-test-1.2-replace-policy-name-in-buffer ()
  (with-temp-buffer
    (insert "AM-Foo AM-Foo-1 X-AM-Foo am-foo q.AM-Foo.x AM-Foo")
    (should (apigee--replace-policy-name-in-buffer "AM-Foo" "N"))
    (should (equal (buffer-string) "N AM-Foo-1 X-AM-Foo am-foo q.N.x N"))
    (should-not (apigee--replace-policy-name-in-buffer "AM-Foo" "N"))))

;;; Template copying (1.3, 1.25)

(ert-deftest apigee-test-1.3-sharedflow-copies-resources ()
  (apigee-tests--with-temp-dir dir
    (let ((b (apigee-tests--new-asset "sharedflow" "s1" "get-googleapis-token" dir)))
      (should (file-exists-p (file-name-concat b "resources" "jsc" "shredJson.js")))
      (should (file-exists-p (file-name-concat b "s1.xml"))))))

(ert-deftest apigee-test-1.3-copy-removes-editor-temp-files ()
  (apigee-tests--with-temp-dir dir
    (let ((src (file-name-concat dir "src"))
          (dest (file-name-concat dir "dest")))
      (dolist (f '("policies/A.xml" "policies/#A.xml#" "policies/A.xml~" "resources/jsc/x.js~"))
        (apigee-tests--write (file-name-concat src f) "x"))
      (make-symbolic-link "nowhere" (file-name-concat src "policies" ".#A.xml"))
      ;; The callers create the destination bundle directory first.
      (make-directory dest)
      (apigee--copy-subdirs '("policies" "resources") src dest)
      (should (equal (directory-files (file-name-concat dest "policies") nil "^[^.]\\|^\\.#")
                     '("A.xml")))
      (should (equal (directory-files (file-name-concat dest "resources" "jsc") nil "^[^.]")
                     nil)))))

(ert-deftest apigee-test-1.25-bare-template ()
  "A template without a name attribute, with an empty DisplayName and no BasePath."
  (apigee-tests--with-temp-dir dir
    (let ((tdir (file-name-concat dir "tmpl" "bare")))
      (apigee-tests--write (file-name-concat tdir "apiproxy" "bare.xml")
                           "<APIProxy><DisplayName/></APIProxy>")
      (apigee-tests--write (file-name-concat tdir "apiproxy" "proxies" "default.xml")
                           "<ProxyEndpoint name=\"default\"><HTTPProxyConnection/></ProxyEndpoint>")
      (apigee--new-asset-from-template "proxy" "p2" (list (list "bare" tdir)) "bare" dir)
      (let ((xml (apigee-tests--read (file-name-concat dir "p2" "apiproxy" "p2.xml"))))
        (should (string-match-p "<APIProxy name=\"p2\">" xml))
        (should (string-match-p "<HTTPProxyConnection/>"
                                (apigee-tests--read (file-name-concat dir "p2" "apiproxy" "proxies" "default.xml"))))
        (should (string-match-p "<DisplayName>p2</DisplayName>" xml))))))

(ert-deftest apigee-test-new-proxy-from-template ()
  "Creating a proxy rewrites name, DisplayName and BasePath, and keeps comments."
  (apigee-tests--with-temp-dir dir
    (let* ((b (apigee-tests--new-asset "proxy" "zz-test" "loopback" dir))
           (defn (apigee-tests--read (file-name-concat b "zz-test.xml"))))
      (should (string-match-p "<APIProxy [^>]*name=[\"']zz-test[\"']" defn))
      (should (string-match-p "<DisplayName>zz-test</DisplayName>" defn))
      (should (string-match-p "<BasePath>/zz-test</BasePath>"
                              (apigee-tests--read (car (apigee--proper-files
                                                        (file-name-concat b "proxies") ".xml"))))))))

;;; Text-based template edits (2.5a)

(defmacro apigee-tests--in-xml (xml &rest body)
  "Run BODY in a temp buffer holding XML; return the buffer text afterwards."
  (declare (indent 1))
  `(with-temp-buffer (insert ,xml) ,@body (buffer-string)))

(ert-deftest apigee-test-2.5a-set-root-attribute ()
  (should (equal (apigee-tests--in-xml "<A revision='1' name='old'>\n  <B name='x'/>\n</A>"
                   (apigee--xml-text-set-root-attribute "name" "new"))
                 "<A revision='1' name='new'>\n  <B name='x'/>\n</A>"))
  (should (equal (apigee-tests--in-xml "<?xml version=\"1.0\"?>\n<!-- <Decoy name='d'> -->\n<A\n   name=\"old\" x=\"a>b\">t</A>"
                   (apigee--xml-text-set-root-attribute "name" "n&m"))
                 "<?xml version=\"1.0\"?>\n<!-- <Decoy name='d'> -->\n<A\n   name=\"n&amp;m\" x=\"a>b\">t</A>"))
  (should (equal (apigee-tests--in-xml "<A/>" (apigee--xml-text-set-root-attribute "name" "n"))
                 "<A name=\"n\"/>"))
  (should (equal (apigee-tests--in-xml "<A xname='q'><B/></A>" (apigee--xml-text-set-root-attribute "name" "n"))
                 "<A name=\"n\" xname='q'><B/></A>")))

(ert-deftest apigee-test-2.5a-set-element-text ()
  (should (equal (apigee-tests--in-xml "<A>\n  <D>old</D>\n  <D>second</D>\n</A>"
                   (should (apigee--xml-text-set-element-text "D" "new")))
                 "<A>\n  <D>new</D>\n  <D>second</D>\n</A>"))
  (should (equal (apigee-tests--in-xml "<A><D k='v'/></A>" (apigee--xml-text-set-element-text "D" "x"))
                 "<A><D k='v'>x</D></A>"))
  (should (equal (apigee-tests--in-xml "<A><Dx>1</Dx><D>\n multi\n line </D></A>"
                   (apigee--xml-text-set-element-text "D" "y"))
                 "<A><Dx>1</Dx><D>y</D></A>"))
  (should (equal (apigee-tests--in-xml "<A/>" (should-not (apigee--xml-text-set-element-text "D" "z")))
                 "<A/>")))

(ert-deftest apigee-test-2.5a-copy-is-lossless ()
  "Copying a template changes only the name, DisplayName and BasePath."
  (apigee-tests--with-temp-dir dir
    (let ((tdir (file-name-concat dir "tmpl" "fancy"))
          (defn "<APIProxy revision='3' name='fancy'>\n    <!-- a comment -->\n    <Description><![CDATA[a < b]]></Description>\n    <DisplayName>fancy</DisplayName>\n</APIProxy>\n")
          (endpoint "<ProxyEndpoint name='default'>\n  <!-- flows go here -->\n  <PreFlow name='PreFlow'>\n    <Request/>\n  </PreFlow>\n  <HTTPProxyConnection>\n    <BasePath>/fancy</BasePath>\n  </HTTPProxyConnection>\n  <RouteRule name='noroute'/>\n</ProxyEndpoint>\n"))
      (apigee-tests--write (file-name-concat tdir "apiproxy" "fancy.xml") defn)
      (apigee-tests--write (file-name-concat tdir "apiproxy" "proxies" "default.xml") endpoint)
      (apigee--new-asset-from-template "proxy" "p4" (list (list "fancy" tdir)) "fancy" dir)
      (let ((b (file-name-concat dir "p4" "apiproxy")))
        (should (equal (apigee-tests--read (file-name-concat b "p4.xml"))
                       (string-trim (string-replace ">fancy<" ">p4<"
                                                    (string-replace "name='fancy'" "name='p4'" defn)))))
        (should (equal (apigee-tests--read (file-name-concat b "proxies" "default.xml"))
                       (string-trim (string-replace "/fancy" "/p4" endpoint))))))))

;;; XML insertion and the inject command (1.4, 1.5, 1.6, 2.5b)

(ert-deftest apigee-test-1.4-insert-child-before ()
  (let ((no-ws (car (apigee--parse-xml "<PE a=\"1\"><Flows/><Route/></PE>"))))
    (apigee--xml-insert-child-before no-ws '(PreFlow nil) '(PostFlow Flows))
    (should (equal no-ws '(PE ((a . "1")) (PreFlow nil) (Flows nil) (Route nil)))))
  (let ((pref (car (apigee--parse-xml "<PE><A/><B/></PE>"))))
    (apigee--xml-insert-child-before pref '(X nil) '(Missing B A))
    (should (equal pref '(PE nil (A nil) (X nil) (B nil)))))
  (let ((none (car (apigee--parse-xml "<PE><A/></PE>"))))
    (apigee--xml-insert-child-before none '(X nil) '(Missing))
    (should (equal none '(PE nil (A nil) (X nil))))))

(defconst apigee-tests--endpoint-shapes
  '(("full" . "<ProxyEndpoint name=\"default\">\n  <PreFlow>\n    <Request/>\n    <Response/>\n  </PreFlow>\n  <PostFlow>\n    <Request/>\n    <Response/>\n  </PostFlow>\n  <Flows/>\n  <HTTPProxyConnection>\n    <BasePath>/a</BasePath>\n  </HTTPProxyConnection>\n  <RouteRule name=\"r\"/>\n</ProxyEndpoint>\n")
    ("no-ws" . "<ProxyEndpoint name=\"default\"><Flows/><HTTPProxyConnection><BasePath>/a</BasePath></HTTPProxyConnection><RouteRule name=\"r\"/></ProxyEndpoint>")
    ("minimal" . "<ProxyEndpoint name=\"default\">\n  <HTTPProxyConnection>\n    <BasePath>/a</BasePath>\n  </HTTPProxyConnection>\n  <RouteRule name=\"r\"/>\n</ProxyEndpoint>\n"))
  "ProxyEndpoint shapes for the inject tests.")

(defun apigee-tests--inject (dir endpoint-xml times)
  "Run the inject command TIMES times on a bundle with ENDPOINT-XML.
Return the parsed ProxyEndpoint element afterwards."
  (let* ((b (apigee-tests--endpoint-bundle dir endpoint-xml))
         (default-directory (file-name-concat b "proxies/")))
    (apigee-tests--with-snippets
     (dotimes (_ times) (apigee-inject-proxy-revision-logic)))
    (car (with-temp-buffer
           (insert-file-contents (file-name-concat b "proxies" "default.xml"))
           (xml-parse-region (point-min) (point-max))))))

(defun apigee-tests--step-names (elt)
  "Return the step names directly under the xml.el node ELT."
  (and elt (apigee--xml-step-names elt)))

(ert-deftest apigee-test-1.4-1.6-inject-places-steps ()
  "Every endpoint shape gets the steps in the right places (1.4, 1.6)."
  (dolist (shape apigee-tests--endpoint-shapes)
    (apigee-tests--with-temp-dir dir
      (let* ((pe (apigee-tests--inject dir (cdr shape) 1))
             (child (lambda (parent name) (car (xml-get-children parent name)))))
        (ert-info ((format "shape: %s" (car shape)))
          (should (equal (apigee-tests--step-names
                          (funcall child (funcall child pe 'PreFlow) 'Response))
                         '("AM-Clean-Request-Headers-From-Response")))
          (should (equal (apigee-tests--step-names
                          (funcall child (funcall child pe 'PostFlow) 'Response))
                         '("AM-Inject-Proxy-Revision-Header")))
          (should (equal (apigee-tests--step-names (funcall child pe 'DefaultFaultRule))
                         '("AM-Inject-Proxy-Revision-Header")))
          (should (equal (xml-node-attributes pe) '((name . "default")))))))))

(ert-deftest apigee-test-1.5-inject-is-idempotent ()
  (dolist (shape apigee-tests--endpoint-shapes)
    (apigee-tests--with-temp-dir dir1
      (apigee-tests--with-temp-dir dir2
        (ert-info ((format "shape: %s" (car shape)))
          (should (equal (apigee-tests--inject dir1 (cdr shape) 1)
                         (apigee-tests--inject dir2 (cdr shape) 2))))))))

;;; Running commands (1.7, 1.13, 1.14, 1.17, 1.19)

(defmacro apigee-tests--with-command-env (&rest body)
  "Run BODY with deterministic programs, org, env, SA and token."
  `(let ((apigee-programs-alist (copy-alist apigee-programs-alist))
         (apigee-organization "my-org")
         (apigee-environment "test")
         (apigee-service-account "-")
         (apigee--gcloud-access-token-cache (lambda (&rest _) "TOK")))
     (setf (alist-get 'apigeecli apigee-programs-alist) "sh"
           (alist-get 'apigeelint apigee-programs-alist) '("sh" "lint.js"))
     ,@body))

(defun apigee-tests--captured-command (command-fn)
  "Call COMMAND-FN with `compilation-start' stubbed; return (COMMAND DIR)."
  (let (captured)
    (cl-letf (((symbol-function 'compilation-start)
               (lambda (command &rest _)
                 (setq captured (list command default-directory))
                 (get-buffer-create " *apigee-test-compilation*"))))
      (funcall command-fn nil))
    captured))

(ert-deftest apigee-test-1.7-sharedflow-lint-and-import ()
  (apigee-tests--with-temp-dir dir
    (apigee-tests--with-command-env
     (let* ((b (apigee-tests--new-asset "sharedflow" "s1" "basic" dir))
            (default-directory (file-name-concat b "policies/")))
       (should (equal (apigee--bundle-name) "s1"))
       (let ((captured (apigee-tests--captured-command #'apigee-lint-asset)))
         (should (string-match-p "lint.js -s \\. " (car captured)))
         (should (equal (cadr captured) (file-name-concat dir "s1/"))))
       ;; Since 2.8, import and deploy work in sharedflows too.
       (should (string-match-p "sharedflows create bundle"
                               (car (apigee-tests--captured-command #'apigee-import-proxy))))))))

(ert-deftest apigee-test-1.19-import-and-deploy-uses-and ()
  (apigee-tests--with-temp-dir dir
    (apigee-tests--with-command-env
     (let ((default-directory (apigee-tests--new-asset "proxy" "p1" "loopback" dir)))
       (should (string-match-p "--token TOK && .*apis deploy"
                               (apigee--get-command 'import-and-deploy nil)))))))

(ert-deftest apigee-test-1.13-resolve-program ()
  (apigee-tests--with-temp-dir dir
    (let* ((sdir (file-name-concat dir "dir with space"))
           (prog (file-name-concat sdir "myprog")))
      (apigee-tests--write prog "#!/bin/sh\necho hi\n")
      (set-file-modes prog #o755)
      (should-not (apigee--resolve-program "no-such-prog-xyz --flag"))
      (should-not (apigee--resolve-program '("no-such-prog-xyz" "a")))
      (should (equal (apigee--resolve-program prog) (list prog)))
      (should (equal (apigee--resolve-program "sh -c true")
                     (list (executable-find "sh") "-c" "true")))
      (should (equal (apigee--resolve-program '("sh" "~/x.js"))
                     (list (executable-find "sh") (expand-file-name "~/x.js")))))))

(ert-deftest apigee-test-1.14-substitute-placeholder ()
  (should (equal (apigee--substitute-placeholder 't "C:\\Users\\me" "x --token %t y")
                 "x --token C:\\Users\\me y"))
  (should (equal (apigee--substitute-placeholder 't "TOK" "%token %t") "%token TOK"))
  (should (equal (apigee--substitute-placeholder 'n "p1" "--name %n") "--name p1")))

(ert-deftest apigee-test-1.17-tokens-masked-in-output-and-recompile ()
  (skip-unless (executable-find "sh"))
  (apigee-tests--with-temp-dir dir
    (let ((apigee-commands-alist (copy-alist apigee-commands-alist)))
      (setf (alist-get 'lint apigee-commands-alist)
            "sleep 0.2; echo leaked --token SECRET123 x; echo done")
      (let ((default-directory (apigee-tests--new-asset "proxy" "p1" "loopback" dir)))
        (apigee-lint-asset nil)
        (let ((buf (get-buffer "*apigeelint - lint - p1*"))
              (wait (lambda (buf)
                      (while (process-live-p (get-buffer-process buf))
                        (accept-process-output nil 0.05))
                      (accept-process-output nil 0.2))))
          (unwind-protect
              (progn
                (funcall wait buf)
                (with-current-buffer buf
                  (should (string-match-p "leaked --token \\*\\*\\*masked\\*\\*\\*" (buffer-string)))
                  (should-not (string-match-p "SECRET123" (buffer-string)))
                  (recompile))
                (funcall wait buf)
                (with-current-buffer buf
                  (should (string-match-p "done" (buffer-string)))
                  (should-not (string-match-p "SECRET123" (buffer-string)))))
            (kill-buffer buf)))
        (should-not (memq 'apigee--mask-secrets (default-value 'compilation-filter-hook)))))))

;;; Command templating (2.4)

(ert-deftest apigee-test-2.4a-placeholders-are-functions ()
  (dolist (entry apigee-placeholders-alist)
    (should (fboundp (cdr entry)))))

(defun apigee-tests--args-printer (dir)
  "Create, in a DIR subdirectory with a space, a script that prints its args."
  (let ((prog (file-name-concat dir "dir with space" "show-args")))
    (apigee-tests--write prog "#!/bin/sh\nfor a in \"$@\"; do echo \"[$a]\"; done\n")
    (set-file-modes prog #o755)
    prog))

(ert-deftest apigee-test-2.4b-words-are-quoted ()
  "A program path and arguments with spaces survive the shell."
  (apigee-tests--with-temp-dir dir
    (apigee-tests--with-command-env
     (let ((default-directory (apigee-tests--new-asset "proxy" "p1" "loopback" dir))
           (apigee-commands-alist '((show . ("%apigeecli" "two words" "%n" "%sa_args" "$HOME")))))
       (setf (alist-get 'apigeecli apigee-programs-alist) (apigee-tests--args-printer dir))
       (setq apigee-service-account "sa@p.iam.gserviceaccount.com")
       (should (equal (shell-command-to-string (apigee--get-command 'show nil))
                      "[two words]\n[p1]\n[--sa]\n[sa@p.iam.gserviceaccount.com]\n[$HOME]\n"))
       (setq apigee-service-account "-")
       (should (equal (shell-command-to-string (apigee--get-command 'show nil))
                      "[two words]\n[p1]\n[$HOME]\n"))))))

(ert-deftest apigee-test-2.4b-sequences-and-string-templates ()
  (apigee-tests--with-temp-dir dir
    (apigee-tests--with-command-env
     (let ((default-directory (apigee-tests--new-asset "proxy" "p1" "loopback" dir))
           (apigee-commands-alist '((a . ("echo" "A" "%n"))
                                    (b . ("echo" "B" "%o"))
                                    (both . (a b))
                                    (legacy . "echo %n %c echo %o; echo done"))))
       (should (equal (apigee--get-command 'both nil) "echo A p1 && echo B my-org"))
       (should (equal (apigee--get-command 'legacy nil) "echo p1 && echo my-org; echo done"))
       (should (equal (shell-command-to-string (apigee--get-command 'legacy nil))
                      "p1\nmy-org\ndone\n"))))))

(ert-deftest apigee-test-2.4c-token-filled-in-after-editing ()
  "The prompt shows %t; the token is inserted only into the command that runs."
  (apigee-tests--with-temp-dir dir
    (apigee-tests--with-command-env
     (let ((default-directory (apigee-tests--new-asset "proxy" "p1" "loopback" dir))
           shown ran)
       (should (string-match-p "--token %t\\'" (apigee--get-command 'import nil t)))
       (should (string-match-p "--token TOK\\'" (apigee--get-command 'import nil)))
       (cl-letf (((symbol-function 'read-shell-command)
                  (lambda (_prompt initial &rest _) (setq shown initial) (concat initial " --extra")))
                 ((symbol-function 'compilation-start)
                  (lambda (command &rest _)
                    (setq ran command)
                    (get-buffer-create " *apigee-test-compilation*"))))
         (apigee-tests--with-answers '("my-org")
           (apigee-import-proxy t)))
       (should (string-match-p "--token %t\\'" shown))
       (should-not (let ((case-fold-search nil)) (string-match-p "TOK" shown)))
       (should (string-match-p "--token TOK --extra\\'" ran))))))

;;; Sharedflow import and deploy (2.8)

(ert-deftest apigee-test-2.8-sharedflow-import-and-deploy ()
  (apigee-tests--with-temp-dir dir
    (apigee-tests--with-command-env
     (let ((default-directory (apigee-tests--new-asset "sharedflow" "s1" "basic" dir)))
       (should (string-match-p
                "sharedflows create bundle -f sharedflowbundle --name s1 -o my-org --token TOK\\'"
                (car (apigee-tests--captured-command #'apigee-import-proxy))))
       (should (string-match-p
                "sharedflows deploy --wait --name s1 --ovr --org my-org --env test --token TOK\\'"
                (car (apigee-tests--captured-command #'apigee-deploy-proxy))))
       (should (string-match-p
                "sharedflows create bundle .* && .*sharedflows deploy "
                (car (apigee-tests--captured-command #'apigee-import-and-deploy-proxy))))))
    (apigee-tests--with-command-env
     (let ((default-directory (apigee-tests--new-asset "proxy" "p1" "loopback" dir)))
       (should (string-match-p "apis create bundle -f apiproxy --name p1 "
                               (car (apigee-tests--captured-command #'apigee-import-proxy))))))))

(ert-deftest apigee-test-2.8-every-kind-maps-every-action ()
  (dolist (kind apigee--asset-kinds)
    (dolist (action '(import deploy import-and-deploy))
      (should (assq (alist-get action (plist-get (cdr kind) :commands))
                    apigee-commands-alist)))))

;;; Guards and prompts (1.8 - 1.12, 1.26)

(ert-deftest apigee-test-1.8-commands-need-a-bundle ()
  (apigee-tests--with-temp-dir dir
    (let ((default-directory dir))
      (apigee-tests--with-answers '("http")
        (should-error (apigee-add-target) :type 'user-error)
        (should-error (apigee-add-policy) :type 'user-error)
        (should-not apigee-tests--prompts))
      (should-not (file-exists-p (file-name-concat dir "apiproxy"))))
    (let ((default-directory (apigee-tests--new-asset "sharedflow" "s1" "basic" dir)))
      (should-error (apigee-add-target) :type 'user-error))))

(ert-deftest apigee-test-1.9-add-target-requires-template ()
  (apigee-tests--with-temp-dir dir
    (let ((default-directory (apigee-tests--new-asset "proxy" "p1" "loopback" dir)))
      (apigee-tests--with-answers '("")
        (should-error (apigee-add-target) :type 'user-error)
        (should (eq (cadr (car apigee-tests--prompts)) t))))))

(ert-deftest apigee-test-1.10-cascaded-choices-ignore-empty-input ()
  (apigee-tests--with-answers '("" "AssignMessage" "" "basic")
    (should (equal (apigee--select-cascaded-choices
                    '(("AssignMessage" ("basic") ("other")) ("Quota" ("q"))))
                   '("AssignMessage" "basic")))))

(ert-deftest apigee-test-1.11-read-or-default ()
  (apigee-tests--with-answers '("" "" "x")
    (should (null (apigee--read-or-default "label" nil)))
    (should (equal (apigee--read-or-default "label" "dflt") "dflt"))
    (should (equal (apigee--read-or-default "label" "dflt") "x"))))

(ert-deftest apigee-test-1.11-organization-required ()
  (let ((apigee-organization nil))
    (apigee-tests--with-answers '("")
      (should-error (apigee-get-organization) :type 'user-error))
    (should (null apigee-organization)))
  (let ((apigee-organization ""))
    (apigee-tests--with-answers '("org-1")
      (should (equal (apigee-get-organization) "org-1")))))

(ert-deftest apigee-test-1.12-no-service-account-is-remembered ()
  (let ((apigee-service-account nil)
        (apigee-organization "my-org"))
    (apigee-tests--with-answers '("-" "-")
      (should (null (apigee-get-service-account-args-for-deployment)))
      (should (null (apigee-get-service-account-args-for-deployment)))
      (should (= (length apigee-tests--prompts) 1)))
    (should (equal apigee-service-account "-"))
    (setq apigee-service-account "deployer")
    (should (equal (apigee-get-service-account-args-for-deployment)
                   '("--sa" "deployer@my-org.iam.gserviceaccount.com")))))

(ert-deftest apigee-test-1.26-new-asset-validates-input ()
  (should-not (commandp 'apigee--new-asset))
  (apigee-tests--with-answers '("")
    (should-error (apigee--new-asset "proxy" nil) :type 'user-error))
  (apigee-tests--with-answers '("p3" "")
    (should-error (apigee--new-asset "proxy" nil) :type 'user-error)
    (should (eq (cadr (car apigee-tests--prompts)) t))))

;;; Paths (1.15, 1.16, 1.22, 1.24)

(ert-deftest apigee-test-1.15-1.16-symlinks ()
  (apigee-tests--with-temp-dir dir
    (apigee-tests--new-asset "proxy" "p1" "loopback" dir)
    (let ((link (file-name-concat dir "p1-link")))
      (make-symbolic-link (file-name-concat dir "p1") link)
      ;; A symlinked containing directory is accepted (1.15).
      (make-symbolic-link dir (file-name-concat dir "home-link"))
      (apigee--new-asset-from-template "proxy" "p5" apigee--proxy-template-alist "loopback"
                                       (file-name-concat dir "home-link"))
      (should (file-directory-p (file-name-concat dir "p5" "apiproxy")))
      (let ((policy (car (apigee--proper-files
                          (file-name-concat link "apiproxy" "policies") ".xml"))))
        (with-current-buffer (find-file-noselect policy)
          (should (equal (apigee--current-policy-name) (file-name-base policy))))))
    (with-temp-buffer
      (let ((default-directory (file-name-concat dir "p1" "apiproxy/")))
        (should-not (apigee--current-policy-name))))))

(ert-deftest apigee-test-1.22-percent-in-error-message ()
  (let ((err (should-error (apigee--verify-exactly-one nil "/tmp/100%dir"))))
    (should (string-match-p "/tmp/100%dir" (error-message-string err)))))

(ert-deftest apigee-test-1.24-containing-dir-candidates ()
  (let ((process-environment (cons "HOME=/tmp/a+b" process-environment))
        (abbreviated-home-dir nil)
        (apigee--recently-used-asset-homes nil)
        (default-directory "/tmp/aab/x/")
        captured)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq captured (all-completions "" collection))
                 "/tmp/aab/x/"))
              ((symbol-function 'make-directory) #'ignore))
      (apigee--prompt-for-containing-dir))
    (should (equal captured '("/tmp/aab/x/")))))

;;; Templates and state (1.20, 1.21, 1.23)

(ert-deftest apigee-test-1.20-1.21-load-templates ()
  (let ((inhibit-message t)
        (messages-start (with-current-buffer (messages-buffer) (point-max))))
    (apigee-load-templates apigee--base-template-dir)
    (should-not (string-match-p "length of list"
                                (with-current-buffer (messages-buffer)
                                  (buffer-substring messages-start (point-max)))))
    (dolist (alist (list apigee--proxy-template-alist apigee--policy-template-alist
                         apigee--target-template-alist apigee--sharedflow-template-alist))
      (let ((names (mapcar #'car alist)))
        (should names)
        (should (equal names (sort (copy-sequence names) #'string<)))))))

(ert-deftest apigee-test-1.23-restore-keeps-existing-values ()
  (apigee-tests--with-temp-dir dir
    (let ((user-emacs-directory dir)
          (apigee-organization "from-init")
          (apigee-environment nil))
      (apigee-tests--write (file-name-concat dir "apigee-el.dat")
                           (prin1-to-string '(("apigee-organization" "from-file")
                                              ("apigee-environment" "env-from-file"))))
      (apigee--restore-state)
      (should (equal apigee-organization "from-init"))
      (should (equal apigee-environment "env-from-file")))))

(ert-deftest apigee-test-1.23-reload-keeps-one-timer ()
  (let ((inhibit-message t))
    (load (expand-file-name "apigee.el" apigee-tests--dir) nil t))
  (should (= 1 (seq-count (lambda (timer) (eq (timer--function timer) 'apigee--persist-state))
                          timer-list)))
  (should (memq 'apigee--persist-state kill-emacs-hook)))

;;; Self-contained package (2.1)

(ert-deftest apigee-test-2.1-no-private-dependencies ()
  "Loading apigee.el pulls in none of the author's private libraries."
  (dolist (feature '(dc-xml dc-sane-sorting xml-to-string))
    (should-not (featurep feature)))
  (should-not (fboundp 'yas-expand-snippet-sync)))

(ert-deftest apigee-test-2.1-completion-table-sorts-alphabetically ()
  (let* ((table (apigee--completion-table '("bb" "a" "ccc")))
         (metadata (completion-metadata "" table nil)))
    (should (eq (completion-metadata-get metadata 'category) 'sorted-sanely))
    (should (equal (funcall (completion-metadata-get metadata 'display-sort-function)
                            '("ccc" "a" "bb"))
                   '("a" "bb" "ccc")))
    (should (equal (all-completions "b" table) '("bb")))))

(ert-deftest apigee-test-2.1-expand-snippet-without-fields ()
  "A snippet with no fields is inserted, and the call returns at once."
  (skip-unless (require 'yasnippet nil t))
  (with-temp-buffer
    (apigee--expand-snippet "<Step><Name>X</Name></Step>")
    (should (equal (buffer-string) "<Step><Name>X</Name></Step>"))))

(ert-deftest apigee-test-2.1-package-requires-header ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "apigee.el" apigee-tests--dir))
    (let ((requires (car (read-from-string (lm-header "Package-Requires")))))
      (should (equal (mapcar #'car requires) '(emacs yasnippet))))))

;;; Bundle lookup (2.2)

(ert-deftest apigee-test-2.2-bundle-lookup ()
  (apigee-tests--with-temp-dir dir
    (apigee-tests--new-asset "proxy" "p1" "loopback" dir)
    (apigee-tests--new-asset "sharedflow" "s1" "basic" dir)
    (let ((default-directory (file-name-concat dir "p1" "apiproxy" "policies/")))
      (should (equal (apigee--bundle) (list :root (file-name-concat dir "p1/") :type "apiproxy")))
      (should (equal (apigee--bundle-subdir (apigee--bundle) "targets")
                     (file-name-concat dir "p1" "apiproxy" "targets/")))
      (should (equal (apigee--bundle-subdir (apigee--bundle))
                     (file-name-concat dir "p1" "apiproxy/"))))
    (let ((default-directory (file-name-concat dir "s1/")))
      (should (equal (plist-get (apigee--bundle) :type) "sharedflowbundle")))
    (let ((default-directory dir))
      (should-not (apigee--bundle)))))

(ert-deftest apigee-test-2.2-commands-signal-outside-a-bundle ()
  (apigee-tests--with-temp-dir dir
    (let ((default-directory dir))
      (apigee-tests--with-answers '("x" "y")
        (should-error (apigee-inject-proxy-revision-logic) :type 'user-error)
        (should-error (apigee-rename-policy) :type 'user-error)
        (should-error (apigee-lint-asset nil) :type 'user-error)
        (should-not apigee-tests--prompts)))
    (let ((default-directory (apigee-tests--new-asset "sharedflow" "s1" "basic" dir)))
      (should-error (apigee-inject-proxy-revision-logic) :type 'user-error))))

(ert-deftest apigee-test-2.2-suggested-names-use-given-dir ()
  (apigee-tests--with-temp-dir dir
    (apigee-tests--write (file-name-concat dir "AM-Set-Header.xml") "<x/>")
    (apigee-tests--write (file-name-concat dir "AM-Set-Header-1.xml") "<x/>")
    (should (equal (apigee--suggested-policy-name dir "AssignMessage" "set header.xml")
                   "AM-Set-Header-2"))
    (apigee-tests--write (file-name-concat dir "http-1.xml") "<x/>")
    (should (equal (apigee--suggested-target-name dir "http") "http-2"))))

;;; Asset kinds (2.3, 3.2)

(ert-deftest apigee-test-2.3-asset-kinds ()
  (should (equal (apigee--asset-kind "proxy" :bundle) "apiproxy"))
  (should (eq (apigee--asset-kind "sharedflow" :templates) 'apigee--sharedflow-template-alist))
  (should-error (apigee--asset-kind "widget" :bundle))
  (dolist (kind apigee--asset-kinds)
    (should (boundp (plist-get (cdr kind) :templates)))
    (let ((after-copy (plist-get (cdr kind) :after-copy)))
      (should (or (null after-copy) (fboundp after-copy))))))

(ert-deftest apigee-test-2.3-no-runtime-built-symbols ()
  "Asset dispatch goes through `apigee--asset-kinds', not `intern' (2.3)."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "apigee.el" apigee-tests--dir))
    (should-not (re-search-forward "(intern (format" nil t))))

(ert-deftest apigee-test-3.2-one-copier-for-both-kinds ()
  "Both kinds go through `apigee--copy-bundle-template'; only proxies get a BasePath."
  (apigee-tests--with-temp-dir dir
    (let (calls)
      (advice-add 'apigee--copy-bundle-template :before
                  (lambda (type &rest _) (push type calls)) '((name . apigee-test)))
      (unwind-protect
          (progn
            (apigee-tests--new-asset "proxy" "p1" "loopback" dir)
            (apigee-tests--new-asset "sharedflow" "s1" "basic" dir))
        (advice-remove 'apigee--copy-bundle-template 'apigee-test))
      (should (equal calls '("sharedflow" "proxy"))))
    (let ((sf (apigee-tests--read (file-name-concat dir "s1" "sharedflowbundle" "s1.xml"))))
      (should (string-match-p "name=\"s1\"" sf))
      (should (string-match-p "<DisplayName>s1</DisplayName>" sf)))))

;;; Shared helpers (3.4 - 3.11)

(ert-deftest apigee-test-3.4-first-available-name ()
  (let ((taken '("a" "a-1" "b-1")))
    (should (equal (apigee--first-available-name "a" (lambda (n) (not (member n taken)))) "a-2"))
    (should (equal (apigee--first-available-name "c" (lambda (n) (not (member n taken)))) "c"))
    (should (equal (apigee--first-available-name "b" (lambda (n) (not (member n taken))) 1) "b-2"))))

(ert-deftest apigee-test-3.5-read-unused-name ()
  (apigee-tests--with-answers '("taken" "also-taken" "free")
    (should (equal (apigee--read-unused-name "Policy" "dflt"
                                             (lambda (n) (equal n "free")))
                   "free"))
    (should (equal (mapcar #'car (reverse apigee-tests--prompts))
                   '("policy name: " "That name is in use. Policy name: "
                     "That name is in use. Policy name: ")))))

(ert-deftest apigee-test-3.6-get-or-prompt ()
  (let ((apigee-lint-exclusions "TD002"))
    (apigee-tests--with-answers '("X")
      (should (equal (apigee-get-lint-exclusions) "TD002"))
      (should-not apigee-tests--prompts)
      (should (equal (apigee-get-lint-exclusions t) "X"))
      (should (equal apigee-lint-exclusions "X")))))

(ert-deftest apigee-test-3.7-3.8-template-alists ()
  (apigee-load-templates apigee--base-template-dir)
  (let ((target (car apigee--target-template-alist))
        (policy (assoc "AssignMessage" apigee--policy-template-alist)))
    (should (equal (car target) (file-name-base (cadr target))))
    (should (file-exists-p (cadr target)))
    (should (cl-every (lambda (f) (string-suffix-p ".xml" f)) (cadr policy)))
    (should (equal (cadr policy) (sort (copy-sequence (cadr policy)) #'string<)))))

(ert-deftest apigee-test-3.11-capitalize-first ()
  "Same as the s-capitalize it replaces: only the first letter is upcased."
  (should (equal (apigee--capitalize-first "x-API-key") "X-api-key"))
  (should (equal (apigee--capitalize-first "") ""))
  (should (equal (apigee--policy-name-elements "AssignMessage" "set Content-Type.xml")
                 '("AM" "Set-Content-type"))))

(ert-deftest apigee-test-3.11-no-s-or-dash ()
  "apigee.el uses only built-ins, seq and cl-lib for strings and sequences."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "apigee.el" apigee-tests--dir))
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (goto-char (point-min))
    (let (found)
      (while (re-search-forward "(\\(s-[a-z]+\\|-[a-z]+\\)[ )]" nil t)
        (unless (nth 4 (syntax-ppss)) (push (match-string 1) found)))
      (should-not found))
    (goto-char (point-min))
    (should-not (re-search-forward "^(require '\\(s\\|dash\\))" nil t))))

;;; Naming (4.2 - 4.6)

(ert-deftest apigee-test-4.2-4.4-old-names-gone ()
  "Misleading or inconsistent names are gone (4.2 - 4.4)."
  (dolist (fn '(apigee--xml-locate-first-child-node apigee--insure-trailing-slash))
    (should-not (fboundp fn)))
  (dolist (cmd '(apigee-lint-asset apigee-validate-xml-for-asset apigee-import-proxy
                 apigee-deploy-proxy apigee-import-and-deploy-proxy
                 apigee-new-proxy apigee-new-sharedflow))
    (should (equal (help-function-arglist cmd t) '(want-prompt))))
  (should (equal (help-function-arglist 'apigee--new-asset t) '(asset-type want-prompt))))

(ert-deftest apigee-test-4.5-token-functions ()
  (should (get 'apigee-gcloud-auth-print-access-token 'byte-obsolete-info))
  (should (eq (indirect-function 'apigee-gcloud-auth-print-access-token)
              (indirect-function 'apigee--gcloud-print-access-token)))
  (should (eq (alist-get 't apigee-placeholders-alist) 'apigee--gcloud-access-token))
  (let ((apigee--gcloud-access-token-cache (lambda () "TOK")))
    (should (equal (apigee--gcloud-access-token t) "TOK"))))

(ert-deftest apigee-test-4.6-shortforms-match-template-dirs ()
  "Every abbreviation is keyed by an existing policy template directory."
  (let ((dirs (directory-files (file-name-concat apigee--base-template-dir "policies")
                               nil "\\`[^.]")))
    (dolist (entry apigee--policytype-shortform)
      (should (member (car entry) dirs)))))

;;; Configuration (2.6)

(defconst apigee-tests--customs
  '(apigee-organization apigee-environment apigee-service-account apigee-lint-exclusions
    apigee-commands-alist apigee-programs-alist apigee-xmlschema-validator-home
    apigee-verbose-logging apigee-timer-minutes)
  "The user options that review item 2.6 made customizable.")

(ert-deftest apigee-test-2.6a-user-options-are-customizable ()
  (require 'wid-edit)
  (let ((members (mapcar #'car (get 'apigee 'custom-group))))
    (dolist (sym apigee-tests--customs)
      (ert-info ((symbol-name sym))
        (should (custom-variable-p sym))
        (should (memq sym members))
        ;; The declared :type accepts the default value.
        (should (widget-apply (widget-convert (get sym 'custom-type))
                              :match (eval (car (get sym 'standard-value)) t)))))))

(ert-deftest apigee-test-2.6a-renamed-variables-have-aliases ()
  (dolist (pair '((apigee--verbose-logging . apigee-verbose-logging)
                  (apigee--timer-minutes . apigee-timer-minutes)
                  (apigee-timer . apigee--timer)
                  (apigee-function-cache . apigee--function-cache)))
    (should (eq (indirect-variable (car pair)) (cdr pair)))
    (should (get (car pair) 'byte-obsolete-variable))))

(ert-deftest apigee-test-2.6b-settings-file-round-trip ()
  "State is written with symbols, and old files with strings still load."
  (apigee-tests--with-temp-dir dir
    (let ((user-emacs-directory dir)
          (apigee-organization nil)
          (apigee-environment nil)
          (apigee-verbose-logging nil)
          (apigee--recently-used-asset-homes nil))
      ;; An old-format file: string names, and an obsolete variable name.
      (apigee-tests--write (file-name-concat dir "apigee-el.dat")
                           (prin1-to-string '(("apigee-organization" "org-a")
                                              ("apigee--verbose-logging" t)
                                              ("apigee--timer-minutes" 99)
                                              ("not-an-apigee-var" "x"))))
      (apigee--restore-state)
      (should (equal apigee-organization "org-a"))
      (should (eq apigee-verbose-logging t))
      (should (= apigee-timer-minutes 6))
      (should-not (boundp 'not-an-apigee-var))
      (setq apigee-environment "env-b")
      (apigee--persist-state)
      (let ((written (with-temp-buffer
                       (insert-file-contents (file-name-concat dir "apigee-el.dat"))
                       (read (current-buffer)))))
        (should (cl-every (lambda (entry) (symbolp (car entry))) written))
        (should (equal (assq 'apigee-environment written) '(apigee-environment "env-b"))))
      (setq apigee-organization nil apigee-environment nil)
      (apigee--restore-state)
      (should (equal (list apigee-organization apigee-environment) '("org-a" "env-b"))))))

;;; Load-time side effects (2.7)

(defun apigee-tests--in-fresh-emacs (form)
  "Evaluate FORM in a fresh `emacs -Q --batch' that has loaded apigee.el.
`user-emacs-directory' there is a new temp dir. Return FORM's value."
  (let ((home (make-temp-file "apigee-fresh" t)))
    (unwind-protect
        (with-temp-buffer
          (call-process (expand-file-name invocation-name invocation-directory)
                        nil t nil "-Q" "--batch"
                        "--eval" (format "(setq load-path '%S)" load-path)
                        "--eval" (format "(setq user-emacs-directory %S)" (file-name-as-directory home))
                        "--eval" (format "(load %S nil t)" (expand-file-name "apigee.el" apigee-tests--dir))
                        "--eval" (format "(prin1 (list :result %S))" form))
          (goto-char (point-min))
          (re-search-forward "^(:result ")
          (goto-char (match-beginning 0))
          (plist-get (read (current-buffer)) :result))
      (delete-directory home t))))

(defconst apigee-tests--side-effects-form
  '(list (seq-count (lambda (timer) (eq (timer--function timer) 'apigee--persist-state)) timer-list)
         (and (memq 'apigee--persist-state kill-emacs-hook) t)
         (and (assq 'apigeelint-visualstudio-error-with-lines compilation-error-regexp-alist-alist) t)
         (and (memq 'apigeelint-visualstudio-error-no-line compilation-error-regexp-alist) t)
         (and apigee--proxy-template-alist t))
  "Timers, exit hook, error regexps and templates set up by apigee.")

(ert-deftest apigee-test-2.7-loading-has-no-side-effects ()
  (should (equal (apigee-tests--in-fresh-emacs apigee-tests--side-effects-form)
                 '(0 nil nil nil nil))))

(ert-deftest apigee-test-2.7-commands-run-setup ()
  "The first command sets everything up, as loading used to (2.7)."
  (should (equal (apigee-tests--in-fresh-emacs
                  `(progn (condition-case nil (apigee-lint-asset nil) (user-error nil))
                          (list apigee--setup-done ,apigee-tests--side-effects-form)))
                 '(t (1 t t t t)))))

(ert-deftest apigee-test-2.7-unload-undoes-setup ()
  (should (equal (apigee-tests--in-fresh-emacs
                  `(progn (apigee-setup)
                          (unload-feature 'apigee t)
                          ;; Its variables are gone now, so leave out the templates check.
                          (list ,(butlast apigee-tests--side-effects-form)
                                (file-exists-p (expand-file-name "apigee-el.dat" user-emacs-directory)))))
                 '((0 nil nil nil) t))))

(ert-deftest apigee-test-2.7-setup-is-idempotent ()
  (let ((apigee--setup-done nil))
    (apigee-setup)
    (apigee-setup))
  (should (= 1 (seq-count (lambda (timer) (eq (timer--function timer) 'apigee--persist-state))
                          timer-list))))

;;; Documentation (5.x)

(ert-deftest apigee-test-5.1-header ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "apigee.el" apigee-tests--dir))
    (should (equal (lm-summary) "Work with Apigee API proxy and sharedflow bundles"))
    (should (lm-code-start))
    (should (string-match-p "lexical-binding: t" (buffer-substring (point-min) (line-end-position))))))

(ert-deftest apigee-test-5.2-commands-are-autoloaded ()
  "Every public interactive command has an autoload cookie."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "apigee.el" apigee-tests--dir))
    (let (missing)
      (mapatoms
       (lambda (sym)
         (when (and (commandp sym)
                    (string-prefix-p "apigee-" (symbol-name sym))
                    (not (string-prefix-p "apigee--" (symbol-name sym)))
                    (not (string-prefix-p "apigee-get-" (symbol-name sym)))
                    (equal (symbol-file sym 'defun)
                           (expand-file-name "apigee.el" apigee-tests--dir)))
           (goto-char (point-min))
           (unless (re-search-forward (concat ";;;###autoload\n(defun " (regexp-quote (symbol-name sym)) " ")
                                      nil t)
             (push sym missing)))))
      (should-not missing))))

(ert-deftest apigee-test-5.3-checkdoc-clean ()
  "checkdoc finds nothing to complain about (single-spaced sentences)."
  (require 'checkdoc)
  (let ((dir (make-temp-file "apigee-checkdoc" t)))
    (unwind-protect
        (with-temp-buffer
          (call-process (expand-file-name invocation-name invocation-directory)
                        nil t nil "-Q" "--batch"
                        "--eval" "(progn (require 'checkdoc) (setq sentence-end-double-space nil))"
                        "--eval" (format "(checkdoc-file %S)"
                                         (expand-file-name "apigee.el" apigee-tests--dir)))
          (should-not (string-match-p "apigee\\.el:[0-9]+:" (buffer-string))))
      (delete-directory dir t))))

;;; Byte compilation (1.18, 2.9)

(defconst apigee-tests--max-compile-warnings 0
  "Byte-compiler warnings allowed for apigee.el. Review item 2.9 got it to 0.")

(defun apigee-tests--compile-warnings ()
  "Byte-compile a copy of apigee.el in a subprocess; return its warning lines."
  (let ((dir (make-temp-file "apigee-compile" t)))
    (unwind-protect
        (progn
          (copy-file (expand-file-name "apigee.el" apigee-tests--dir)
                     (file-name-concat dir "apigee.el"))
          (with-temp-buffer
            (call-process (expand-file-name invocation-name invocation-directory)
                          nil t nil "-Q" "--batch"
                          "--eval" (format "(setq load-path '%S)" load-path)
                          "-f" "batch-byte-compile" (file-name-concat dir "apigee.el"))
            (should (file-exists-p (file-name-concat dir "apigee.elc")))
            (seq-filter (lambda (line) (string-match-p "Warning:" line))
                        (split-string (buffer-string) "\n"))))
      (delete-directory dir t))))

(ert-deftest apigee-test-2.9-compile-warnings-do-not-grow ()
  "Guard against new byte-compiler warnings, and against 1.18 regressing."
  (let ((warnings (apigee-tests--compile-warnings)))
    (should-not (seq-filter (lambda (w) (string-match-p "tramp-\\|re1\\|count-of-changes" w))
                            warnings))
    (should (<= (length warnings) apigee-tests--max-compile-warnings))))

(provide 'apigee-tests)

;;; apigee-tests.el ends here
