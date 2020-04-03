;;; apigee-edge.el --- utility functions for working with Apigee Edge platform in emacs
;;
;; Copyright (C) 2017-2020 Dino Chiesa and Google, LLC.
;;
;; Author     : Dino Chiesa
;; Maintainer : Dino Chiesa <dchiesa@google.com>
;; Created    : May 2017
;; Modified   : April 2020
;; Version    : 1.1
;; Keywords   : apigee edge
;; Requires   : s.el, xml.el
;; License    : Apache 2.0
;; X-URL      : https://github.com/DinoChiesa/unknown...
;; Last-saved : <2020-April-02 17:33:18>
;;
;;; Commentary:
;;
;; This module defines a few elisp functions that are handy for working
;; with API Proxy definition bundles within emacs. Per ejemplo,
;;  - creating a new blank proxy from a template
;;  - adding a new policy to a bundle
;;
;; Future possibilities:
;;  - adding a new target to a bundle
;;  - zipping and importing a bundle
;;  - validating a bundle
;;
;; To use this module, put this in your .emacs file:
;;  (add-to-list 'load-path "~/elisp/apigee")
;;  (require 'apigee-edge)
;;
;; Then, you can invoke various commands:
;;
;; To add a new proxy:
;;   M-x edge-new-proxy
;;
;; To specify the directory in which to store the new proxy:
;;   C-u M-x edge-new-proxy
;;
;; To add a policy to an existing proxy, open a dired buffer to ../apiproxy and:
;;   M-x edge-add-policy
;;
;;
;;; License
;;
;;    Copyright 2017-2020 Google LLC.
;;
;;    Licensed under the Apache License, Version 2.0 (the "License");
;;    you may not use this file except in compliance with the License.
;;    You may obtain a copy of the License at
;;
;;        https://www.apache.org/licenses/LICENSE-2.0
;;
;;    Unless required by applicable law or agreed to in writing, software
;;    distributed under the License is distributed on an "AS IS" BASIS,
;;    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;    See the License for the specific language governing permissions and
;;    limitations under the License.
;;
;;; TODO: see todo file

(require 's) ;; magnars' long lost string library
(require 'xml)
(require 'dash) ;; magnars' functional lib

(defvar edge--base-template-dir nil
  "The directory from which policy templates have been most recently loaded")

(defvar edge--recently-used-asset-homes nil
  "a list of directories recently used to store API Proxies.")

(defvar edge--verbose-logging nil
  "whether this module should log verbosely into *Messages*. This is an on/off variable. Set it to a truthy value to get logging.")

(defvar edge--timer-minutes 24
  "length of the interval in minutes between persisting Edge-emacs settings.")

(defvar edge-timer nil
  "cancellable timer, for saving Edge-emacs settings.")

(defvar edge--list-of-vars-to-store-and-restore
  (list "edge--verbose-logging" "edge--recently-used-asset-homes" "edge--base-template-dir" "edge--timer-minutes")
  "a list of variables to store/restore in the settings file.")

(defvar edge--settings-file-base-name "apigee-edge.dat")

(defvar edge--load-file-name load-file-name
  "The name from which the Apigee Edge module was loaded.")

(defconst edge--policytype-shortform
  (list
   '("AssignMessage" "AM")
   '("AccessEntity" "AE")
   '("ServiceCallout" "SC")
   '("XSL" "XSL")
   '("AccessControl" "AC")
   '("Script" "PY")
   '("LookupCache" "Cache")
   '("PopulateCache" "Cache")
   '("InvalidateCache" "Cache")
   '("ResponseCache" "Cache")
   '("RaiseFault" "RF")
   '("MessageLogging" "ML")
   '("GenerateSAMLAssertion" "SAML")
   '("ValidateSAMLAssertion" "SAML")
   '("VerifyApiKey" "VerifyApiKey")
   '("JavaCallout" "Java")
   '("JavaScript" "JS")
   '("ExtractVariables" "EV")
   '("OAuthV2" "OAuthV2")
   '("XMLToJSON" "XMLToJSON")
   ;; '("VerifyJWT" "JWT")
   ;; '("GenerateJWT" "JWT")
   ;; '("DecodeJWT" "JWT")
   '("JSONToXML" "JSONToXML")
   '("GetOAuthV2Info" "OAuthV2-GetInfo")
   '("SetOAuthV2Info" "OAuthV2-SetInfo")
   '("DeleteOAuthV2Info" "OAuthV2-DeleteInfo")
   '("BasicAuthentication" "BA")))

(defconst edge--http-status-message-alist
  (list
   '("200" "OK")
   '("201" "Created")
   '("302" "Moved")
   '("304" "Not Modified")
   '("400" "Bad Request")
   '("401" "Not Authorized")
   '("403" "Forbidden")
   '("404" "Not Found")
   '("410" "Gone")
   '("429" "Too Many Requests")
   '("500" "Server Error")
   '("501" "Not Implemented")
   '("503" "Server Busy")))

(defconst edge--message-payload-sample-alist
  (list
   '("application/json" "{
  \"status\" : true,
  \"message\" : \"whatever\",
  \"clientId\" : \"{parsedRequest.client_id}\"
}
")
   '("application/x-www-form-urlencoded" "status=true&clientId={parsedRequest.client_id}")
   '("text/plain" "ok")
   '("application/xml" "<message><here>{parsedRequest.client_id}</here></message>")
   '("soap" "<soap:Envelope xmlns:soap='http://schemas.xmlsoap.org/soap/envelope/' xmlns:ser='http://avatax.avalara.com/services'>
  <soap:Header>
    <wsse:Security xmlns:wsse='http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd'
                   soap:actor='http://schemas.xmlsoap.org/soap/actor/next'>
      <wsse:UsernameToken>
        <wsse:Username>{avalara.username}</wsse:Username>
        <wsse:Password>{avalara.username}</wsse:Password>
      </wsse:UsernameToken>
    </wsse:Security>
    <ser:Profile>
      <ser:Name>Apigee TEST</ser:Name>
      <ser:Client>Apigee Edge Client</ser:Client>
      <ser:Adapter>SOAP API</ser:Adapter>
      <ser:Machine>MBPRO</ser:Machine>
    </ser:Profile>
  </soap:Header>
  <soap:Body>
    <ser:Ping/>
  </soap:Body>
</soap:Envelope>
")))


(defvar edge--policy-template-alist nil
  "An alist. For each element, the cons is a policy type name, eg \"AccessEntity\", and the cadr is a list of strings, each of which is a filename referring to a template for the policy. The actual filename will be in (edge--join-path-elements edge--base-template-dir \"policies\" policy-type filename)")

(defvar edge--proxy-template-alist nil
  "An alist, relating a name to a proxy template. The cadr of each entry is a string, a directory name, containing the exploded proxy template.")

(defvar edge--target-template-alist nil
  "An alist, relating a name to a target template. The cadr of each entry is a string, a file name, containing the target template.")

(defvar edge--sharedflow-template-alist nil
  "An alist, relating a name to a template for a sharedflow. The cadr of each entry is a string, a directory name, containing the exploded sharedflow template.")

(defun edge--path-to-settings-file ()
  "a function rturning the path to the settings file for apigee-edge.el"
  (edge--join-path-elements user-emacs-directory edge--settings-file-base-name))

(defun edge--restore-state ()
  "function expected to be called on initial module load, that restores the previous state of the module. Things like the most recently used apiproxy home, or the most recently loaded templates directory."
  (let ((dat-file-path (edge--path-to-settings-file)))
    (with-temp-buffer
      (insert-file-contents dat-file-path)
      (save-excursion
        (goto-char (point-min))
        (let ((settings-data (read (current-buffer))))
          (dolist (one-setting settings-data)
            (let ((setting-name (car one-setting)))
              (if (and
                   (member setting-name edge--list-of-vars-to-store-and-restore)
                   (cadr one-setting))
                  (set (intern setting-name) (cadr one-setting))))))))))

(defun edge--persist-state ()
  "function expected to be called periodically to store the state of the module. Things like the most recently used apiproxy home, or the most recently loaded templates directory."
  (setq edge--recently-used-asset-homes (edge--fresh-recent-asset-homes t))
  (let ((dat-file-path (edge--path-to-settings-file))
        (alist-to-write nil))
    (dolist (setting-name edge--list-of-vars-to-store-and-restore)
      (push (list setting-name (eval (intern setting-name))) alist-to-write))
    (with-temp-file dat-file-path
      (goto-char (point-min))
      (erase-buffer)
      (insert ";; settings for apigee-edge.el")
      (newline)
      (insert (concat ";; Stored: " (current-time-string)))
      (newline)
      (let ((print-length nil)) ;; to avoid truncating
        (pp alist-to-write (current-buffer))))))  ;; print

(defun edge--join-path-elements (root &rest dirs)
  "Joins a series of directories together, inserting slashes as necessary,
like Python's os.path.join."
  (if (not dirs)
      root
    (apply 'edge--join-path-elements
           (let ((first (car dirs)))
             (if (s-suffix? "/" root)
                 (concat root
                         (if (s-prefix? "/" first)
                             (substring first 1 (length first))
                           first))
               (if (s-prefix? "/" first)
                   (concat root first)
                 (concat root "/" (car dirs)))))
           (cdr dirs))))

(defun edge--insure-trailing-slash (path)
  "Insure the given path ends with a slash. This is usedful with
`default-directory'. Setting `default-directory' to a value that
does not end with a slash causes it to use the parent directory.
"
  (and path
       (if (s-ends-with? "/" path) path (concat path "/"))))

(defun edge--proper-subdirs (containing-dir)
  "Return list of full paths of proper subdirs found in CONTAINING-DIR."
  (remove-if (lambda (file)
               (let ((clean-name (file-name-nondirectory file)))
                 (or (string-match "^\\." clean-name)
                     (string-match "node_modules" clean-name)
                     (not (file-directory-p file)))))
             (directory-files containing-dir t)))

(defun edge--proper-files (containing-dir &optional suffix)
  "Return list of full paths of proper files found in CONTAINING-DIR.
Optionally filters on files with the given extension or SUFFIX."
  (remove-if (lambda (file)
               (let ((clean-name (file-name-nondirectory file)))
                 (or
                  (and suffix
                       (not (s-ends-with? suffix clean-name)))
                  (string-match "^\\." clean-name)
                  (string-match "^#.*#$" clean-name)
                  (string-match "~$" clean-name)
                  (file-directory-p file))))
             (directory-files containing-dir t)))

(defun edge--sort-strings (strings)
  "lexicographically sort a list of STRINGS"
  (sort strings
        (lambda (a b) (string< a b ))))

(defun edge--sort-by-string-car (list-o-lists)
  "sort LIST-O-LISTS, a list of lists, each of the form (STRING (LIST)),
lexicographically by the car of each element, which is a string."
  (sort list-o-lists
        (lambda (a b) (string< (car a) (car b) ))))

(defun edge--get-target-template-contents (template-filename)
  "return the contents of the target template file.
"
  (let ((filename
         (if (s-starts-with? "/" template-filename)
             template-filename
         (edge--join-path-elements edge--base-template-dir "targets" template-filename))))
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun edge--get-policy-template-contents (template-filename)
  "return the contents of the policy template file.
"
  (let ((filename
         (edge--join-path-elements edge--base-template-dir "policies" template-filename)))
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun edge--templates-for-one-policy-type (policy-type-directory)
  "loads the policy templates for one POLICY-TYPE-DIRECTORY, which is a fully-qualified
directory, ending in a policy-type name eg AccessEntity.  This is used to
populate the menu of available policy templates."
  (let (templ-list)
    (dolist (this-template (edge--proper-files policy-type-directory ".xml"))
      (push (file-name-nondirectory this-template) templ-list))
    (list (file-name-nondirectory policy-type-directory)
          (edge--sort-strings templ-list))))

(defun edge--policy-template-count ()
  "returns the total number of policy templates available in
`edge--policy-template-alist'"
  (apply '+
          (mapcar (lambda (item) (length (cadr item)))
                  edge--policy-template-alist)))

(defun edge--load-asset-templates (sym label subdir)
  "Load templates for a kind of asset from the template dir, then set the symbol to the alist."
  (let ((top-level-dir (edge--join-path-elements edge--base-template-dir subdir )))
    (set sym
         (let (template-list)
           (dolist (template-name (edge--proper-subdirs top-level-dir))
             (push
              (list (file-name-nondirectory template-name) template-name)
              template-list)
             (and edge--verbose-logging (message "%s template %s" label template-name)))
           (reverse (edge--sort-by-string-car template-list))))
    (list label (length (symbol-value sym)) top-level-dir)))

(defun edge-load-proxy-templates ()
  "Load proxy templates from the proxy template dir. "
  (edge--load-asset-templates 'edge--proxy-template-alist "proxy" "proxies"))

(defun edge-load-sharedflow-templates ()
  "Load templates for sharedflows from the template dir. "
  (edge--load-asset-templates 'edge--sharedflow-template-alist "sharedflow" "sharedflows"))

(defun edge-load-policy-templates ()
  "Load policy templates from the policy template dir.

Each child under the template directory should have the name of a policy-type.
Within that child dir, one or more template files, each ending in .xml,
which contain templates for the policy. The name of the file will be
the name of the template in the resulting menu.
"
  (let ((top-level-dir (edge--join-path-elements edge--base-template-dir "policies")))
    (setq
        edge--policy-template-alist
        (let (template-list)
          (dolist (policy-type (edge--proper-subdirs top-level-dir))
            (push
             (edge--templates-for-one-policy-type policy-type)
             template-list)
            (and edge--verbose-logging (message "policy type %s" policy-type)))
          (reverse (edge--sort-by-string-car template-list))))
    (list "policy" (edge--policy-template-count) top-level-dir)))


(defun edge-load-target-templates ()
  "Load target templates from the target template dir.

Each child under the template directory should be a .xml file defining a target.
The name of the file will be the name of the template in the resulting menu.
"
  (let ((top-level-dir (edge--join-path-elements edge--base-template-dir "targets" )))
    (setq edge--target-template-alist
         (let (template-list)
           (dolist (template-fname (edge--proper-files top-level-dir ".xml"))
             (push
              (list (edge--trim-xml-suffix (file-name-nondirectory template-fname)) template-fname)
              template-list)
             (and edge--verbose-logging (message "target template %s" template-fname)))
           (let ((v (length template-list)))
             (message "length of list %d" v))
           (reverse (edge--sort-by-string-car template-list))))
    (list "target" (length edge--target-template-alist) top-level-dir)))


(defun edge-load-templates (top-level-dir &optional interactive)
  "Load templates for proxies and policies from the top level directory TOP-LEVEL-DIR.

Under TOP-LEVEL-DIR there should be sub-directories named \"policies\", \"proxies\",
and \"sharedflows\".
Within each of those, there will be templates for those entities.
"
  (interactive
   (list
    (read-directory-name
     "the template dir?: "
     edge--base-template-dir edge--base-template-dir t)
    t))
  (setq edge--base-template-dir top-level-dir)
  (let ((results (list (edge-load-proxy-templates)
                       (edge-load-policy-templates)
                       (edge-load-target-templates)
                       (edge-load-sharedflow-templates)
                       )))
    (when interactive
      (message "Loaded templates: [%s] from %s"
               (mapconcat
                (lambda (x) (format "%d %s" (cadr x) (car x)))
                results
                ", ")
               top-level-dir))))

(defun edge--trim-xml-suffix (s)
  "trims the .xml suffix from a template file name"
  (s-chop-suffix ".xml" s))

(defun edge--get-menu-position ()
  "get the position for the popup menu"
  (if (fboundp 'posn-at-point)
      (let ((x-y (posn-x-y (posn-at-point (point)))))
        (list (list (+ (car x-y) 10)
                    (+ (cdr x-y) 20))
              (selected-window)))
    t))

(defun edge--find-bundle (flavor) ;; "apiproxy" or "sharedflowbundle"
  (let ((right-here (concat (file-name-directory default-directory) flavor)))
    (if (edge--is-existing-directory right-here)
        (file-name-directory default-directory)
      (let ((elts (reverse (split-string (file-name-directory default-directory) "/")))
            r)
        (while (and elts (not r))
          (if (string= (car elts) flavor)
              (setq r (reverse (cdr elts)))
            (setq elts (cdr elts))))
        (if r
            (list (mapconcat 'identity r "/") flavor ))))))

(defun edge--type-of-bundle ()
  "returns \"apiproxy\" or \"sharedflowbundle\", or nil"
  (cadr (cl-some 'edge--find-bundle '("apiproxy" "sharedflowbundle"))))

(defun edge--root-path-of-bundle ()
  "Returns the path of the directory that contains the
apiproxy directory.

If the apiproxy is defined in a structure like this:

~/foo/bar/APINAME/apiproxy
~/foo/bar/APINAME/apiproxy/APINAME.xml
~/foo/bar/APINAME/apiproxy/resources
~/foo/bar/APINAME/apiproxy/resources/...
~/foo/bar/APINAME/apiproxy/targets
~/foo/bar/APINAME/apiproxy/targets/..
~/foo/bar/APINAME/apiproxy/proxies
~/foo/bar/APINAME/apiproxy/proxies/..
..

... and this function is invoked from anywhere in one of those directories,
then the return value is: ~/foo/bar/APINAME/

It always ends in slash.

"
    (let ((path
           (edge--insure-trailing-slash
            (car (cl-some 'edge--find-bundle '("apiproxy" "sharedflowbundle"))))))
      (and path (file-truename path))))

(defun edge--target-name-is-available (tname)
  "is a target by this name available? Eg, does a file NOT exist with this name?"
  (let ((filename-to-check
         (concat (edge--root-path-of-bundle) "apiproxy/targets/" tname ".xml")))
    (not (file-exists-p filename-to-check))))


(defun edge--suggested-target-name (template-name)
  "suggest a name for a target given a TEMPLATE-NAME for the target.
Based on file availability."
  (let ((val 1)
        (next-name (lambda (v) (concat template-name "-" (format "%d" v)))))
      (let ((tname (funcall next-name val)))
        (while (not (edge--target-name-is-available tname))
          (setq val (1+ val)
                tname (funcall next-name val)))
        tname)))


;;;###autoload
(defun edge-add-target ()
  "Invoke this interactively, and the fn will prompt the user to
choose a target type to insert.
"
  (interactive)
  (let ((apiproxy-dir (edge--root-path-of-bundle))
        (template-name
           (ido-completing-read
            (format "target template: ")
            (mapcar (lambda (x) (car x)) edge--target-template-alist)
            nil nil nil)))
    (when template-name
      (let ((target-dir (concat apiproxy-dir "apiproxy/targets/"))
            (have-name nil)
            (target-name-prompt "target name: ")
            (choice (assoc template-name edge--target-template-alist)))
        (let* ((template-filename (nth 1 choice))
              (raw-template (edge--get-target-template-contents template-filename)))
          (and (not (file-exists-p target-dir))
               (make-directory target-dir))

          ;;(asset-name (read-string (format "%s name?: " asset-type) default-value nil default-value)))

            (let* ((default-value (edge--suggested-target-name template-name))
                  (target-name
                   (let (n)
                     (while (not have-name)
                       (setq n (read-string target-name-prompt default-value nil default-value)
                             have-name (edge--target-name-is-available n)
                             target-name-prompt "That name is in use. Target name: " ))
                      n))
                   (elaborated-template
                    (progn
                      (while (string-match "##" raw-template)
                        (setq raw-template (replace-match target-name t t raw-template)))
                      raw-template)))

              ;; create the file, expand the snippet, save it.
              (find-file (concat target-dir target-name ".xml"))
              ;; yas-expand-snippet-sync does not return until the snip is expanded.
              (yas-expand-snippet-sync elaborated-template (point) (point))
              (save-buffer)
              ;; here, optionally open the resource file, if any
              (kill-new
               (concat "<RouteRule name='foo'><Target>" target-name "</Target></RouteRule>"))
              (message "yank to add the RouteRule declaration...")
              ))))))


(defun edge--generate-policy-menu ()
  "From the list of candidates, generate a keymap suitable for
use as a menu in `popup-menu' . Each item in the input list of
candidates is a list, (POLICY-TYPE (TEMPLATE TEMPLATE...)), where
POLICY-TYPE is one of {Quota, XMLToJSON, Javascript, etc}, and TEMPLATE
is the name of the file containing a template to fill in for a
new policy. Like this:
  ((\"AccessEntity\"
    (\"app.xml\" \"basic.xml\" \"developer.xml\"))
   (\"AssignMessage\"
    (\"Set Content-Type.xml\" \"Store Original header.xml\"))
   (\"BasicAuthentication\"
    (\"Decode Inbound.xml\" \"Encode Outbound.xml\"))
   ...)

The output is a keymap representing a multi-leveled hierarchy, like this:

  (keymap
    ...
   (BasicAuthentication \"BasicAuthentication\" keymap
                        (12 \"Decode Inbound\" . 12)
                        (11 \"Encode Outbound\" . 11)
                        \"BasicAuthentication\")
   (AssignMessage \"AssignMessage\" keymap
                  (10 \"assign variable\" . 10)
                  (9 \"clean response headers\" . 9)
                  (8 \"full response\" . 8)
                  (7 \"remove query param or header\" . 7)
                  (6 \"Set Content-Type\" . 6)
                  (5 \"set query param and-or headers\" . 5)
                  (4 \"Store Original header\" . 4)
                  \"AssignMessage\")
   (AccessEntity \"AccessEntity\" keymap
                 (3 \"app\" . 3)
                 (2 \"basic\" . 2)
                 (1 \"developer\" . 1)
                 \"AccessEntity\")
   \"Insert a policy...\")


The intention is to provide input to `popup-menu', to display a cascading,
multi-level popup menu.

"
  (let ((candidates edge--policy-template-alist))
    (let ((keymap (make-sparse-keymap "Insert a policy..."))
          (n 0) ;; need for cons cell
          (j 0)
          (len (length candidates))
          (sort-by-name (lambda (a b) (not (string< (downcase a) (downcase b))))))

      (while (< j len)
        (let* ((template-set (nth j candidates))
               (cat (car template-set))
               (templates (sort (copy-sequence (cadr template-set)) sort-by-name)))
          (define-key keymap (vector (intern cat)) (cons cat (make-sparse-keymap cat)))
          (dolist (template templates)
            (define-key keymap
              (vector (intern cat) (edge--join-path-elements cat template))
              (cons (edge--trim-xml-suffix template) n))
            ))

        (setq j (1+ j)))
      keymap)))


(defun edge--prompt-user-with-policy-choices ()
  "Prompt the user with the available choices.
In this context the available choices is the hierarchical list
of available policies.
"
  (interactive)
    ;; NB:
    ;; x-popup-menu displays in the proper location, near
    ;; the cursor.
    ;;
    ;; x-popup-dialog always displays in the center
    ;; of the frame, which makes for an annoying
    ;; user-experience.
    (x-popup-menu (edge--get-menu-position)
                  (edge--generate-policy-menu)))

(defun edge--is-existing-directory (dir-name)
  "Tests to see whether a name refers to an existing directory."
  (and
   (file-exists-p dir-name)
   (let ((attrs (file-attributes dir-name)))
     (and
      (car attrs)
      (not (stringp (car attrs)))))))




(defun edge--fixup-script-name (name &optional prefix)
  "returns a stripped name suitable for use for a file in the resources/jsc directory,
or resources/py, or resources/xsl."

  (let* ((default-prefix "Javascript")
         (real-prefix (concat (downcase (if (stringp prefix) prefix default-prefix)) "-"))
         (pos (length real-prefix)))
    (if (and (>= (length name) (length real-prefix))
             (string= real-prefix (downcase (substring name 0 pos))))
        (let ((s (substring name pos)))
          (concat (downcase (substring s 0 1)) (substring s 1)))
      name)))

(defun edge--policy-name-is-available (pname)
  "Return true if the passed policy name PNAME is unused, in other words
if no file exists by that name in the given proxy.
"
  (let ((filename-to-check
         (concat (edge--root-path-of-bundle) (edge--type-of-bundle) "/policies/" pname ".xml")))
    (not (file-exists-p filename-to-check))))

(defun edge--suggested-policy-name (ptype filename)
  "Returns a string that contains a default policy name, uses a counter
that is indexed per policy type within each API Proxy.
"
  (if (or (string= ptype "JWT") (string= ptype "JWS"))
      (let ((shortname (car (reverse (split-string filename "/")))))
        (setq ptype (concat (s-capitalize (car (split-string shortname " "))) ptype))))

  (let ((ptype (or (cadr (assoc ptype edge--policytype-shortform)) ptype))
        (val 1)
        (next-name (lambda (v) (concat ptype "-" (format "%d" v)))))
      (let ((pname (funcall next-name val)))
        (while (not (edge--policy-name-is-available pname))
          (setq val (1+ val)
                pname (funcall next-name val)))
        pname)))

;;;###autoload
(defun edge-add-policy ()
  "Invoke this interactively, and the fn will prompt the user to
choose a policy type to insert. It will then ask for a name for
the policy, create the appropriate XML file, and using
yas-snippet, expand the template associated to the chosen policy,
into the policy file. It then will open any resource files as
appropriate.

"
  (interactive)
  (let ((bundle-dir (edge--root-path-of-bundle))
        (bundle-type (edge--type-of-bundle))
        (choice (edge--prompt-user-with-policy-choices)))
    (when choice
      (let ((num-items (length choice)))
        (when (eq num-items 2)
          ;; policy-type (nth 0 choice)
          ;; template-file (nth 1 choice)
          (let ((policy-dir (concat bundle-dir bundle-type "/policies/"))
                (ptype (symbol-name (nth 0 choice)))
                (template-filename (nth 1 choice))
                (have-name nil)
                (policy-name-prompt "policy name: "))
            (and (not (file-exists-p policy-dir))
                 (make-directory policy-dir))
            (let* ((raw-template (edge--get-policy-template-contents template-filename))
                   (default-value (edge--suggested-policy-name ptype template-filename))
                   (policy-name
                    (let (n)
                      (while (not have-name)
                        (setq n (read-string policy-name-prompt default-value nil default-value)
                              have-name (edge--policy-name-is-available n)
                              policy-name-prompt "That name is in use. Policy name: " ))
                      n))

                   (elaborated-template
                    (progn
                      (while (string-match "##" raw-template)
                        (setq raw-template (replace-match policy-name t t raw-template)))
                      raw-template)))

              ;; create the file, expand the snippet, save it.
              (find-file (concat policy-dir policy-name ".xml"))
              ;; yas-expand-snippet-sync does not return until the snip is expanded.
              (yas-expand-snippet-sync elaborated-template (point) (point))
              (save-buffer)
              ;;(apigee-mode 1)
              ;; here, optionally open the resource file, if any
              (cond
               ((or (string= ptype "JavaScript") (string= ptype "XSL") (string= ptype "Python"))
                (save-excursion
                  (goto-char (point-min))
                  (if (re-search-forward "<ResourceURL>\\(jsc\\|xsl\\|py\\)://\\(.+\\)</ResourceURL>" (point-max) t)
                      (let ((resource-type (match-string-no-properties 1))
                            (resource-basename (match-string-no-properties 2)))
                        (if resource-basename
                            (let ((resource-dir
                                   (concat bundle-dir bundle-type "/resources/" resource-type "/")))
                              (and (not (file-exists-p resource-dir))
                                   (make-directory resource-dir t))
                              (find-file-other-window (concat resource-dir resource-basename))
                              ;;(apigee--maybe-insert-base-content resource-basename resource-type)
                              ;;(apigee-mode 1)
                              ))))))
               (t nil))
              (kill-new policy-name)
              (kill-new
               (concat "<Step><Name>" policy-name "</Name></Step>"))
              (message "yank to add the step declaration...")
              )))))))

;; (defun edge-maybe-sync-policy-filename ()
;;   "synchronizes the name of the file with the name specified in the name
;; attribute in the root element, if the file is a policy file.
;;
;; This function is normally used as an after-save-hook, and normally only in an
;; xml-mode (such as `nxml-mode'), since policy files are XML files.
;;
;; With this hook, the human can modify the name attribute inside the XML, and
;; save the file; and then that change gets reflected in the filename for the buffer
;; and also in every other file in the API-proxy bundle.
;; "
;;   (interactive)
;;   (let ((orig-filename (buffer-file-name)))
;;     (if (apigee--is-policy-file orig-filename)
;;         (let* ((orig-policyname (file-name-sans-extension (file-name-nondirectory orig-filename)))
;;                (root (xml-parse-region))
;;                (policy (car root))
;;                (attrs (xml-node-attributes policy))
;;                (new-policyname (cdr (assq 'name attrs))))
;;           (if (and new-policyname
;;                    (not (s-equals-p orig-policyname new-policyname)))
;;               (let* ((new-short-filename (concat new-policyname ".xml"))
;;                      (new-filename
;;                       (concat (file-name-directory orig-filename) new-short-filename)))
;;                 (rename-file orig-filename new-filename 1)
;;                 (rename-buffer new-short-filename t) ;; get unique buffer name
;;                 (set-visited-file-name new-filename) ;; sets modified flag t
;;                 (set-buffer-modified-p nil)
;;                 ;; now, search/replace all files in the apiproxy to replace
;;                 ;; that old policy name with the new policy name.
;;                 (let ((policy-buffer-name (buffer-name))
;;                       (file-list (apigee-all-files-in-apiproxy))
;;                       (re1 (concat "\\<" (regexp-quote orig-policyname) "\\>")))
;;                   (while file-list
;;                     (let* ((one-file (car file-list))
;;                            (base-filename (file-name-nondirectory one-file)))
;;                       (if (not (or (s-starts-with? "#" base-filename)
;;                                    (s-starts-with? ".#" base-filename)))
;;                           (with-current-buffer (find-file-noselect one-file)
;;                             (save-excursion
;;                               (goto-char (point-min))
;;                               (let ((need-save nil)
;;                                     (original-modified (buffer-modified-p)))
;;                                 (while (re-search-forward re1 nil t)
;;                                   (replace-match new-policyname)
;;                                   (setq need-save t))
;;                                 (if (and need-save
;;                                          (not original-modified)
;;                                          (not (s-equals-p policy-buffer-name (buffer-name))))
;;                                     (save-buffer))))))
;;                       (setq file-list (cdr file-list)))))))))))

(defun edge--cleanup-newlines ()
  "collapse multiple newlines"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(\n\s*\\)\\([\s]*\n\\)" (point-max) t)
      (replace-match (match-string 1)))))

(defun edge--cleanup-quotes ()
  "replace escaped quote with quote character"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([ =]\\)&quot;" (point-max) t)
      (replace-match (concat (match-string 1) "\"")))
    (goto-char (point-min))
    (while (re-search-forward "&quot;\\([ <)]\\)" (point-max) t)
      (replace-match (concat "\"" (match-string 1))))))

(defun edge--serialize-xml-elt (elt)
  "Serialize an XML element into the current buffer, and then collapse newlines."
  (erase-buffer)
  (xml-print elt)
  (edge--cleanup-newlines)
  (edge--cleanup-quotes)
  (save-excursion
    (indent-region (point-min) (point-max)))
  )

(defun edge--verify-exactly-one (xml-file-list source-dir)
  "verifies that there is exactly one xml file."
      (if (not xml-file-list)
          (error
           (message "[apigee-edge] cannot find XML file in %s, in `edge--verify-exactly-one'" source-dir)))
      (if (not (= (length xml-file-list) 1))
          (error
           (message "[apigee-edge] found more than one XML file in %s, in `edge--verify-exactly-one'" source-dir))))

(defun edge--copy-subdirs (subdirs source-dir dest-dir)
  (while subdirs
        (let* ((this-dir (car subdirs))
               (source-subdir (edge--join-path-elements source-dir this-dir))
               (dest-subdir (edge--join-path-elements dest-dir this-dir)))
          (if (edge--is-existing-directory source-subdir)
              (copy-directory source-subdir dest-subdir t nil)))
        (setq subdirs (cdr subdirs))))

(defun edge--copy-proxy-template-files (proxy-name source destination)
  "copy files from the SOURCE template directory to the DESTINATION directory,
changing names and replacing / expanding things as appropriate."
  (let ((source-apiproxy-dir (edge--join-path-elements source "apiproxy"))
        (dest-apiproxy-dir (edge--join-path-elements destination "apiproxy")))

    (let ((xml-file-list (edge--proper-files source-apiproxy-dir ".xml")))
      (edge--verify-exactly-one xml-file-list source-apiproxy-dir)
      ;; create the toplevel destination directory
      (make-directory dest-apiproxy-dir t)
      ;; copy, then intelligently modify the toplevel proxy bundle definition file
      (let ((new-xml-file-name (edge--join-path-elements dest-apiproxy-dir (concat proxy-name ".xml"))))
        (copy-file (car xml-file-list) new-xml-file-name nil t t nil)
        (with-current-buffer (find-file-noselect new-xml-file-name)
          (let*
              ((root (xml-parse-region (point-min) (point-max)))
               (apiproxy-elt (car root))
               (apiproxy-elt-attrs (xml-node-attributes apiproxy-elt))
               (display-name-elt (car (xml-get-children apiproxy-elt 'DisplayName))))
            (setcdr (assq 'name apiproxy-elt-attrs) proxy-name)
            (setcar (cddr display-name-elt) proxy-name)
            (edge--serialize-xml-elt root))
          (save-buffer))))

    (edge--copy-subdirs (list "proxies" "policies" "resources" "targets") source-apiproxy-dir dest-apiproxy-dir)

    ;; if we can find a single XML file in the proxies dir, modify it
    (let* ((dest-proxyendpoints-dir (edge--join-path-elements dest-apiproxy-dir "proxies"))
           (xml-file-list (edge--proper-files dest-proxyendpoints-dir ".xml")))
      (if (not xml-file-list)
          (error
           (message "[apigee-edge] cannot find XML file in %s, in `edge--copy-proxy-template-files'" dest-proxyendpoints-dir)))
      (if (= (length xml-file-list) 1)
          (with-current-buffer (find-file-noselect (car xml-file-list))
            (let*
                ((root (xml-parse-region (point-min) (point-max)))
                 (proxy-endpoint-elt (car root))
                 (http-proxy-conn-elt (car (xml-get-children proxy-endpoint-elt 'HTTPProxyConnection)))
                 (basepath-elt (car (xml-get-children http-proxy-conn-elt 'BasePath))))
              (setcar (cddr basepath-elt) (concat "/" proxy-name))
              (edge--serialize-xml-elt root)
              (edge--cleanup-quotes))
            (save-buffer 0))))
    ))

(defun edge--copy-sharedflow-template-files (sf-name source destination)
  "copy files from the SOURCE template directory to the DESTINATION directory,
changing names and replacing / expanding things as appropriate."
  (let ((source-sf-dir (edge--join-path-elements source "sharedflowbundle"))
        (dest-sf-dir (edge--join-path-elements destination "sharedflowbundle")))
    (let ((xml-file-list (edge--proper-files source-sf-dir ".xml")))
      (edge--verify-exactly-one xml-file-list source-sf-dir)
      (make-directory dest-sf-dir t)
      ;; copy, then intelligently modify the toplevel proxy bundle definition file
      (let ((new-xml-file-name (edge--join-path-elements dest-sf-dir (concat sf-name ".xml"))))
        (copy-file (car xml-file-list) new-xml-file-name nil t t nil)
        (with-current-buffer (find-file-noselect new-xml-file-name)
          (let*
              ((root (xml-parse-region (point-min) (point-max)))
               (sfbundle-elt (car root))
               (sfbundle-elt-attrs (xml-node-attributes sfbundle-elt))
               (display-name-elt (car (xml-get-children sfbundle-elt 'DisplayName))))
            (setcdr (assq 'name sfbundle-elt-attrs) sf-name)
            (setcar (cddr display-name-elt) sf-name)
            (edge--serialize-xml-elt root))
          (save-buffer))))
    (edge--copy-subdirs (list "sharedflows" "policies") source-sf-dir dest-sf-dir)))

(defun edge--new-asset-from-template (asset-type asset-name template-alist template-name containing-dir)
  "Non-interactive function to create a new asset from a template.
ASSET-TYPE - either \"proxy\" or \"sharedflow\".
ASSET-NAME - name of the to-be-created asset.
TEMPLATE-ALIST - the alist of templates.
TEMPLATE-NAME - the name of the asset template, as stored in the templates/<asset-type> directory.
CONTAINING-DIR - name of an existing directory into which to insert the new asset.
"
  (if (not (edge--is-existing-directory containing-dir))
    (error
     (message "[apigee-edge] containing-dir does not exist in `edge--new-asset-from-template'")))
  (let ((template-match (assoc template-name template-alist))
        (new-dir (edge--join-path-elements containing-dir asset-name)))
    (if (edge--is-existing-directory new-dir)
        (error
         (message "[apigee-edge] new-dir already exists in `edge--new-asset-from-template'")))
    (if (not template-match)
        (error
         (message "[apigee-edge] unknown template in `edge--new-asset-from-template'")))
    (let ((template-dir (cadr template-match))
          (copy-fn (intern (format "edge--copy-%s-template-files" asset-type))))
      (funcall copy-fn asset-name template-dir new-dir))
    (find-file-existing new-dir)))

(defun edge--fresh-recent-asset-homes (with-time)
  "returns the list of recently used asset homes, eliminating stale entries, in sorted order.
If WITH-TIME is non-nil, emit the list with time (suitable for persisting). Otherwise without -
it will be a bare list of strings representing directory paths."
  (let ((result
         (let ((days-considered-stale 30))
           (-sort (lambda (a b)
                    (> (float-time (time-subtract (apply 'encode-time (cadr a)) (apply 'encode-time (cadr b)))) 0))
                  (-filter (lambda (x)
                             (edge--time-is-within-days (apply 'encode-time (cadr x)) days-considered-stale))
                           edge--recently-used-asset-homes)))))
    (if with-time result
      (-map (lambda (n) (car n)) result))))

(defun edge--time-is-within-days (the-time delta-days)
  "Returns t if THE-TIME is less than DELTA-DAYS ago."
  (let ((delta-seconds (* 24 delta-days 60 60)))
    (< (float-time (time-subtract (current-time) the-time)) delta-seconds)))

(defun edge--prompt-for-containing-dir ()
  "prompt user for a containing directory, and return it."
  (let ((homedir (concat (getenv "HOME") "/"))
        (candidate-list (edge--fresh-recent-asset-homes nil)))
    (ido-completing-read
     "containing directory?: "
     (mapcar (lambda (x) (replace-regexp-in-string homedir "~/" x))
             (delq nil (delete-dups candidate-list))) nil nil nil)))

(defun edge-new-proxy (arg)
  "Interactive fn that creates a new exploded proxy bundle directory
structure. Prompts for the name of the API Proxy, and the base template
for the proxy.

When invoked with a prefix, this fn will prompt the user also for
the name of the directory in which to store the apiproxy.

When invoked without a prefix, it uses the most recent asset home
directory. If no directory has ever been used, it prompts for the directory.

"
  (interactive "P")
  (edge--new-asset "proxy" arg))


(defun edge-new-sharedflow (arg)
  "Interactive fn that creates a new exploded sharedflow bundle directory
structure. Prompts for the name of the sharedflow, and the base template
for the thing.

When invoked with a prefix, this fn will prompt the user also for
the name of the directory in which to store the sharedflow.

When invoked without a prefix, it uses the most recent asset home
directory. If no directory has ever been used, it prompts for the directory.

"
  (interactive "P")
  (edge--new-asset "sharedflow" arg))


(defun edge--remember-asset-home (containing-dir)
  "stores one directory with the current time in the list"
  (setq edge--recently-used-asset-homes
        (-remove (lambda (x)  (equal (car x) containing-dir)) edge--recently-used-asset-homes))
  (setq edge--recently-used-asset-homes (cons (list containing-dir (decode-time nil t))
                                              edge--recently-used-asset-homes)))


(defun edge--new-asset (asset-type prompt-arg)
  "Internal interactive fn that creates a new exploded asset directory
structure. Prompts for the name of the asset, and the base template
for the thing.

When invoked with a prefix, this fn will prompt the user also for
the name of the directory in which to store the asset.

When invoked without a prefix, it uses the most recent asset home
directory. If no directory has ever been used, it prompts for the directory.

This fn is not intended to be called directly.

ASSET-TYPE - either \"proxy\" or \"sharedflow\"
PROMPT-ARG - whether invoked with a prefix

"
  (interactive "P")
  (let ((alist-sym (intern (format "edge--%s-template-alist" asset-type))))
    (let ((asset-name (read-string (format "%s name?: " asset-type) nil nil nil))
          (template
           (ido-completing-read
            (format "%s template: " asset-type)
            (mapcar (lambda (x) (car x)) (symbol-value alist-sym))
            nil nil nil))
          (containing-dir
           (if prompt-arg
               (edge--prompt-for-containing-dir)
             (or (car (edge--fresh-recent-asset-homes nil)) (edge--prompt-for-containing-dir)))))
      (edge--remember-asset-home containing-dir)
      (edge--new-asset-from-template asset-type asset-name (symbol-value alist-sym) template containing-dir))))


;; restore last known state, and set a timer to persist state periodically
(eval-after-load "apigee-edge"
  '(progn
     (edge--restore-state)
     (if (not edge--base-template-dir)
         (setq edge--base-template-dir (concat (file-name-directory edge--load-file-name) "templates/")))
     (if edge--base-template-dir
         (edge-load-templates edge--base-template-dir))
     (setq edge-timer
           (run-with-timer (* 60 edge--timer-minutes) (* 60 edge--timer-minutes) 'edge--persist-state))))


(provide 'apigee-edge)

;;; apigee-edge.el ends here
