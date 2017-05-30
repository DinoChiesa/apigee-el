;;; apigee-edge.el --- utility functions for working with Apigee Edge platform in emacs
;;
;; Copyright (C) 2017 Dino Chiesa and Google, Inc.
;;
;; Author     : Dino Chiesa
;; Maintainer : Dino Chiesa <dchiesa@google.com>
;; Created    : May 2017
;; Modified   : June 2017
;; Version    : 1.0
;; Keywords   : apigee edge
;; Requires   : s.el, xml.el
;; License    : Apache 2.0
;; X-URL      : https://github.com/DinoChiesa/unknown...
;; Last-saved : <2017-May-30 16:45:25>
;;
;;; Commentary:
;;
;; This module defines a few elisp functions that are handy for working
;; with API Proxy definition bundles within emacs. Per ejemplo,
;;  - creating a new blank proxy
;;  - zipping and importing a bundle
;;  - adding a new policy to a bundle
;;  - adding a new target to a bundle
;;  - retrieving / updating a single resource in an existing API Proxy
;;  - validating a bundle (coming soon)
;;
;;; License
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;;; TODO: see todo file

(require 's) ;; magnars' long lost string library
(require 'xml)

(defvar edge--base-template-dir nil
  "The directory from which policy templates have been most recently loaded")

(defvar edge--recently-used-apiproxy-homes nil
  "a list of directories recently used to store API Proxies.")

(defvar edge--verbose-logging nil
  "whether this module should log verbosely into *Messages*. This is an on/off variable. Set it to a truthy value to get logging.")

(defvar edge--list-of-vars-to-store-and-restore
  (list "edge--verbose-logging" "edge--recently-used-apiproxy-homes" "edge--base-template-dir")
  "a list of variables to store/restore in the sttings file.")

(defvar edge--settings-file-base-name "apigee-edge.dat")

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
   '("Javascript" "JS")
   '("ExtractVariables" "EV")
   '("OAuthV2" "OAuth")
   '("XMLToJSON" "XMLToJSON")
   '("JSONToXML" "JSONToXML")
   '("GetOAuthV2Info" "OAuth")
   '("SetOAuthV2Info" "OAuth")
   '("DeleteOAuthV2Info" "OAuth")
   '("BasicAuthentication" "BA")))


(defvar edge--policy-template-alist nil
  "An alist. For each element, the cons is a policy type name, eg \"AccessEntity\", and the cadr is a list of strings, each of which is a filename referring to a template for the policy. The actual filename will be in (edge--join-path-elements edge--base-template-dir \"policies\" policy-type filename)")

(defvar edge--proxy-template-alist nil
  "An alist, relating a name to a proxy template. The cadr of each entry is a string, a directory name, containing the exploded proxy template.")


(defun edge--path-to-settings-file ()
  "a function rturning the path to the settings file for apigee-edge.el"
  (edge--join-path-elements user-emacs-directory edge--settings-file-base-name))

;; (defun edge--get-string-from-file (file-path)
;;   "Return content from file at FILE-PATH. It should be fully-qualified."
;;   (with-temp-buffer
;;     (insert-file-contents file-path)
;;     (buffer-string)))

(defun edge--restore-state ()
  "function expected to be called on initial module load, that restores the previous state of the module. Things like the most recently used apiproxy home, or the most recently loaded templates directory."
  (let ((dat-file-path (edge--path-to-settings-file)))
    (with-current-buffer (find-file-noselect dat-file-path)
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

(defun edge--get-template-contents (ptype template-filename)
  "return the contents of the policy template file"
  (let ((filename
         (edge--join-path-elements (edge--policy-template-dir) template-filename)))
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

(defun edge--template-count ()
  "returns the total number of policy templates available in
`edge--policy-template-alist'"
  (apply '+
          (mapcar (lambda (item) (length (cadr item)))
                  edge--policy-template-alist)))

(defun edge-load-proxy-templates ()
  "Load proxy templates from the proxy template dir. "
  (let ((top-level-dir (edge--join-path-elements edge--base-template-dir "proxies")))
    (setq
        edge--proxy-template-alist
        (let (template-list)
          (dolist (proxy-template-name (edge--proper-subdirs top-level-dir))
            (push
             (list (file-name-nondirectory proxy-template-name) proxy-template-name)
             template-list)
            (and edge--verbose-logging (message "proxy template %s" proxy-template-name)))
          (reverse (edge--sort-by-string-car template-list))))
    (list (length edge--proxy-template-alist) top-level-dir)))

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
    (list (edge--template-count) top-level-dir)))

(defun edge-load-templates (top-level-dir &optional interactive)
  "Load templates for proxies and policies from the top level directory TOP-LEVEL-DIR.

Under TOP-LEVEL-DIR there should be sub-directories named \"policies\" and \"proxies\".
Within each of those, there will be templates for those entities.
"
  (interactive
   (list
    (read-directory-name
     "the template dir?: "
     edge--base-template-dir edge--base-template-dir t)
    t))
  (setq edge--base-template-dir top-level-dir)
  (let ((results (list (edge-load-proxy-templates) (edge-load-policy-templates) )))
    (when interactive
      (message "Loaded templates: [%d proxy, %d policy] from %s."
               (car (car results))
               (car (cadr results))
               top-level-dir))))

(defun edge--trim-suffix (s)
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

(defun edge--generate-policy-menu (candidates)
  "From the list of candidates, generate a keymap suitable for
use as a menu in `popup-menu' . Each item in the input list of
CANDIDATES is a list, (POLICY-TYPE (TEMPLATE TEMPLATE...)), where
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
              (cons (edge--trim-suffix template) n))
            ))

      (setq j (1+ j)))
    keymap))


;; (let ((keymap (make-sparse-keymap "Insert a policy..."))
;;         (n 0) ;; need for cons cell
;;         (j 0)
;;         (len (length candidates))
;;         (sort-by-name (lambda (a b) (not (string< (downcase a) (downcase b))))))
;;
;;     (while (< j len)
;;       (let* ((template-set (nth j candidates))
;;              (cat (car template-set))
;;              (templates (sort (copy-sequence (cadr template-set)) sort-by-name)))
;;
;;         (if (= (length templates) 1)
;;             (let ((template (car templates)))
;;               (define-key keymap (vector (intern cat) (intern template))
;;                 (cons (edge--trim-suffix template) n)))
;;
;;           (define-key keymap (vector (intern cat)) (cons cat (make-sparse-keymap cat)))
;;           (dolist (template templates)
;;             (define-key keymap (vector (intern cat) (intern template)) (cons (edge--trim-suffix template) n)))))
;;
;;       (setq j (1+ j)))
;;
;;     ;; this works with popup-menu
;;     keymap)





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
                  (edge--generate-policy-menu edge--policy-template-alist)))


(defun edge--is-existing-directory (dir-name)
  "Tests to see whether a name refers to an existing directory."
  (and
   (file-exists-p dir-name)
   (let ((attrs (file-attributes dir-name)))
     (and
      (car attrs)
      (not (stringp (car attrs)))))))


(defun edge--path-of-apiproxy ()
  "Returns the path of the directory that contains the
apiproxy directory.

If the apiproxy is defined in a structure like this:

~/dev/apiproxies/APINAME/apiproxy
~/dev/apiproxies/APINAME/apiproxy/APINAME.xml
~/dev/apiproxies/APINAME/apiproxy/resources
~/dev/apiproxies/APINAME/apiproxy/resources/...
~/dev/apiproxies/APINAME/apiproxy/targets
~/dev/apiproxies/APINAME/apiproxy/targets/..
~/dev/apiproxies/APINAME/apiproxy/proxies
~/dev/apiproxies/APINAME/apiproxy/proxies/..
..

... and this function is invoked from anywhere in one of those directories,
then the return value is: ~/dev/apiproxies/APINAME/

It always ends in slash.

"
  (let ((path
         (edge--insure-trailing-slash
          (let ((maybe-this (concat (file-name-directory default-directory) "apiproxy")))
            (if (edge--is-existing-directory maybe-this)
                (file-name-directory default-directory)
              (let ((elts (reverse (split-string (file-name-directory default-directory) "/")))
                    r)
                (while (and elts (not r))
                  (if (string= (car elts) "apiproxy")
                      (setq r (reverse (cdr elts)))
                    (setq elts (cdr elts))))
                (if r
                    (mapconcat 'identity r "/") )))))))
    (and path (file-truename path))))


(defun edge--policy-name-is-available (pname)
  "Return true if the passed policy name PNAME is unused, in other words
if no file exists by that name in the given proxy.
"
  (let ((filename-to-check
         (concat (edge--path-of-apiproxy) "apiproxy/policies/" pname ".xml")))
    (not (file-exists-p filename-to-check))))


(defun edge--suggested-policy-name (ptype)
  "Returns a string that contains a default policy name, uses a counter
that is indexed per policy type within each API Proxy.
"
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
  (let ((apiproxy-dir (edge--path-of-apiproxy))
        (choice (edge--prompt-user-with-policy-choices)))
    (when choice
      (let ((num-items (length choice)))
        (when (eq num-items 2)
          ;; policy-type (nth 0 choice)
          ;; template-file (nth 1 choice)
          (let ((policy-dir (concat apiproxy-dir "apiproxy/policies/"))
                (ptype (symbol-name (nth 0 choice)))
                (template-filename (nth 1 choice))
                (have-name nil)
                (policy-name-prompt "policy name: "))
            (and (not (file-exists-p policy-dir))
                 (make-directory policy-dir))
            (let* ((raw-template (edge--get-template-contents ptype template-filename))
                   (default-value (edge--suggested-policy-name ptype))
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
              (apigee-mode 1)
              ;; here, optionally open the resource file, if any
              (cond
               ((or (string= ptype "Javascript") (string= ptype "XSL") (string= ptype "Python"))
                (save-excursion
                  (goto-char (point-min))
                  (if (re-search-forward "<ResourceURL>\\(jsc\\|xsl\\|py\\)://\\(.+\\)</ResourceURL>" (point-max) t)
                      (let ((resource-type (match-string-no-properties 1))
                            (resource-basename (match-string-no-properties 2)))
                        (if resource-basename
                            (let ((resource-dir
                                   (concat apiproxy-dir "apiproxy/resources/" resource-type "/")))
                              (and (not (file-exists-p resource-dir))
                                   (make-directory resource-dir))
                              (find-file-other-window (concat resource-dir resource-basename))
                              (apigee--maybe-insert-base-content resource-basename resource-type)
                              (apigee-mode 1)))))))
               (t nil))
              (kill-new policy-name)
              (kill-new
               (concat "<Step><Name>" policy-name "</Name></Step>"))
              (message "yank to add the step declaration...")
              )))))))

(defun edge--copy-proxy-template-files (proxy-name source destination)
  "copy files from the SOURCE template directory to the DESTINATION directory,
changing names and replacing / expanding things as appropriate."
  (let ((source-apiproxy-dir (edge--join-path-elements source "apiproxy"))
        (dest-apiproxy-dir (edge--join-path-elements destination "apiproxy"))
        (serialize-xml-elt (lambda (elt)
                             (erase-buffer)
                             (xml-print elt)
                             (goto-char (point-min))
                             (while (re-search-forward "\n\\([\s]*\n\\)+" (point-max) t)
                               (replace-match "\n")))))
    ;; verify that there is exactly one XML file.
    (let ((xml-file-list (edge--proper-files source-apiproxy-dir ".xml")))
      (if (not xml-file-list)
          (error
           (message "[apigee-edge] cannot find XML file in %s, in `edge--copy-proxy-template-files'" source-apiproxy-dir)))
      (if (not (= (length xml-file-list) 1))
          (error
           (message "[apigee-edge] found more than one XML file in %s, in `edge--copy-proxy-template-files'" source-apiproxy-dir)))
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
            (funcall serialize-xml-elt root))
          (save-buffer))))
    ;; copy all the sub-directories
    (let ((subdirs (list "proxies" "policies" "resources" "targets")))
      (while subdirs
        (let* ((this-dir (car subdirs))
               (source-subdir (edge--join-path-elements source-apiproxy-dir this-dir))
               (dest-subdir (edge--join-path-elements dest-apiproxy-dir this-dir)))
          (copy-directory source-subdir dest-subdir t nil))
        (setq subdirs (cdr subdirs))))
    ;; finally, if we can find a single XML file in the proxies dir, modify it
    (let* ((dest-proxies-dir (edge--join-path-elements dest-apiproxy-dir "proxies"))
           (xml-file-list (edge--proper-files dest-proxies-dir ".xml")))
      (if (not xml-file-list)
          (error
           (message "[apigee-edge] cannot find XML file in %s, in `edge--copy-proxy-template-files'" dest-proxies-dir)))
      (if (= (length xml-file-list) 1)
          (with-current-buffer (find-file-noselect (car xml-file-list))
            (let*
                ((root (xml-parse-region (point-min) (point-max)))
                 (proxy-endpoint-elt (car root))
                 (http-proxy-conn-elt (car (xml-get-children proxy-endpoint-elt 'HTTPProxyConnection)))
                 (basepath-elt (car (xml-get-children http-proxy-conn-elt 'BasePath))))
              (setcar (cddr basepath-elt) (concat "/" proxy-name))
              (funcall serialize-xml-elt root))
            (save-buffer))))
    ))



(defun edge-new-proxy-from-template (proxy-name template-name containing-dir)
  "Non-interactive function to create a new proxy.
PROXY-NAME should be the name of the to-be-created proxy.
TEMPLATE-NAME should be the name of the proxy template, as stored in the templates/proxies directory.
CONTAINING-DIR is the name of an existing directory in which to insert the new proxy.
"
  (if (not (edge--is-existing-directory containing-dir))
    (error
     (message "[apigee-edge] containing-dir does not exist in `edge-new-proxy-from-template'")))
  (let ((template-match (assoc template-name edge--proxy-template-alist))
        (new-proxy-dir (edge--join-path-elements containing-dir proxy-name)))
    (if (edge--is-existing-directory new-proxy-dir)
        (error
         (message "[apigee-edge] new-proxy-dir already exists in `edge-new-proxy-from-template'")))
    (if (not template-match)
        (error
         (message "[apigee-edge] unknown proxy template in `edge-new-proxy-from-template'")))
    (let ((template-dir (cadr template-match)))
      (edge--copy-proxy-template-files proxy-name template-dir new-proxy-dir))
    (find-file-existing new-proxy-dir)))

(defun edge--prompt-for-containing-dir ()
  "prompt user for a containing directory, and return it."
  (let ((homedir (concat (getenv "HOME") "/"))
        (candidate-list edge--recently-used-apiproxy-homes))
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

When invoked without a prefix, it uses the most recent apiproxy home
directory. If no directory has ever been used, it prompts for the directory.

"
  (interactive "P")
  (let ((proxy-name (read-string "proxy name?: " nil nil nil))
        (proxy-template
         (ido-completing-read
          "proxy template: "
          (mapcar (lambda (x) (car x)) edge--proxy-template-alist)
          nil nil nil))
        (proxy-containing-dir
         (if arg
             (edge--prompt-for-containing-dir)
           (or (car edge--recently-used-apiproxy-homes) (edge--prompt-for-containing-dir)))))
    (edge-new-proxy-from-template proxy-name proxy-template proxy-containing-dir)))



;;; restore last known state
(eval-after-load "apigee-edge"
  '(progn
     (edge--restore-state)
     (if edge--base-template-dir
         (edge-load-templates edge--base-template-dir))
     ))


(provide 'apigee-edge)

;;; apigee-edge.el ends here