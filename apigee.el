;;; apigee.el --- utility functions for working with Apigee platform in emacs
;;
;; Copyright (C) 2017-2021 Dino Chiesa and Google, LLC.
;;
;; Author     : Dino Chiesa
;; Maintainer : Dino Chiesa <dchiesa@google.com>
;; Created    : May 2017
;; Modified   : February 2021
;; Version    : 1.1
;; Keywords   : apigee
;; Requires   : s.el, xml.el
;; License    : Apache 2.0
;; X-URL      : https://github.com/DinoChiesa/apigee-el
;; Last-saved : <2021-July-01 10:53:11>
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
;; To use this module, assuming ~/elisp/apigee contains apigee.el,
;; put this in your .emacs file:
;;  (add-to-list 'load-path "~/elisp/apigee")
;;  (require 'apigee)
;;
;; Then, you can invoke various commands:
;;
;; To add a new proxy:
;;   M-x apigee-new-proxy
;;
;; To specify the directory in which to store the new proxy:
;;   C-u M-x apigee-new-proxy
;;
;; To add a policy to an existing proxy, open a dired buffer to ../apiproxy and:
;;   M-x apigee-add-policy
;;
;;
;;; License
;;
;;    Copyright 2017-2021 Google LLC.
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
(require 'dash) ;; magnars' functional lib, functions start with dash

(defvar apigee--base-template-dir nil
  "The directory from which policy templates have been most recently loaded")

(defvar apigee--recently-used-asset-homes nil
  "a list of directories recently used to store API Proxies.")

(defvar apigee--verbose-logging nil
  "whether this module should log verbosely into *Messages*. This is an on/off variable. Set it to a truthy value to get logging.")

(defvar apigee--timer-minutes 6
  "length of the interval in minutes between persisting apigee-emacs settings.")

(defvar apigee-timer nil
  "cancellable timer, for saving apigee-emacs settings.")

(defvar apigee--list-of-vars-to-store-and-restore
  (list "apigee--verbose-logging" "apigee--recently-used-asset-homes" "apigee--base-template-dir" "apigee--timer-minutes")
  "a list of variables to store/restore in the settings file.")

(defvar apigee--settings-file-base-name "apigee-edge.dat")

(defvar apigee--load-file-name load-file-name
  "The name from which the Apigee module was loaded.")

(defconst apigee--policytype-shortform
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
   '("FlowCallout" "FC")
   '("JavaCallout" "Java")
   '("JavaScript" "JS")
   '("ExtractVariables" "EV")
   '("OAuthV2" "OAuthV2")
   '("XMLToJSON" "X2J")
   '("CORS" "CORS")
   ;; '("VerifyJWT" "JWT")
   ;; '("GenerateJWT" "JWT")
   ;; '("DecodeJWT" "JWT")
   '("JSONToXML" "J2X")
   '("GetOAuthV2Info" "OAuthV2-GetInfo")
   '("SetOAuthV2Info" "OAuthV2-SetInfo")
   '("DeleteOAuthV2Info" "OAuthV2-DeleteInfo")
   '("BasicAuthentication" "BA")))

(defconst apigee--http-status-message-alist
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


(defconst apigee--message-payload-sample-alist
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


(defvar apigee--policy-template-alist nil
  "An alist. For each element, the cons is a policy type name, eg \"AccessEntity\", and the cadr is a list of strings, each of which is a filename referring to a template for the policy. The actual filename will be in (apigee--join-path-elements apigee--base-template-dir \"policies\" policy-type filename)")

(defvar apigee--proxy-template-alist nil
  "An alist, relating a name to a proxy template. The cadr of each entry is a string, a directory name, containing the exploded proxy template.")

(defvar apigee--target-template-alist nil
  "An alist, relating a name to a target template. The cadr of each entry is a string, a file name, containing the target template.")

(defvar apigee--sharedflow-template-alist nil
  "An alist, relating a name to a template for a sharedflow. The cadr of each entry is a string, a directory name, containing the exploded sharedflow template.")

(defun apigee--path-to-settings-file ()
  "a function rturning the path to the settings file for apigee.el"
  (apigee--join-path-elements user-emacs-directory apigee--settings-file-base-name))

(defun apigee--restore-state ()
  "function expected to be called on initial module load, that restores the previous state of the module. Things like the most recently used apiproxy home, or the most recently loaded templates directory."
  (let ((dat-file-path (apigee--path-to-settings-file)))
    (with-temp-buffer
      (insert-file-contents dat-file-path)
      (save-excursion
        (goto-char (point-min))
        (let ((settings-data (read (current-buffer))))
          (dolist (one-setting settings-data)
            (let ((setting-name (car one-setting)))
              (if (and
                   (member setting-name apigee--list-of-vars-to-store-and-restore)
                   (cadr one-setting))
                  (set (intern setting-name) (cadr one-setting))))))))))

(defun apigee--persist-state ()
  "function expected to be called periodically to store the state of the module. Things like the most recently used apiproxy home, or the most recently loaded templates directory."
  (setq apigee--recently-used-asset-homes (apigee--fresh-recent-asset-homes t))
  (let ((dat-file-path (apigee--path-to-settings-file))
        (alist-to-write nil))
    (dolist (setting-name apigee--list-of-vars-to-store-and-restore)
      (push (list setting-name (eval (intern setting-name))) alist-to-write))
    (with-temp-file dat-file-path
      (goto-char (point-min))
      (erase-buffer)
      (insert ";; settings for apigee.el")
      (newline)
      (insert (concat ";; Stored: " (current-time-string)))
      (newline)
      (let ((print-length nil)) ;; to avoid truncating
        (pp alist-to-write (current-buffer))))))  ;; print

(defun apigee--join-path-elements (root &rest dirs)
  "Joins a series of directories together, inserting slashes as necessary,
like Python's os.path.join."
  (if (not dirs)
      root
    (apply 'apigee--join-path-elements
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

(defun apigee--insure-trailing-slash (path)
  "Insure the given path ends with a slash. This is usedful with
`default-directory'. Setting `default-directory' to a value that
does not end with a slash causes it to use the parent directory.
"
  (and path
       (if (s-ends-with? "/" path) path (concat path "/"))))

(defun apigee--proper-subdirs (containing-dir)
  "Return list of full paths of proper subdirs found in CONTAINING-DIR."
  (remove-if (lambda (file)
               (let ((clean-name (file-name-nondirectory file)))
                 (or (string-match "^\\." clean-name)
                     (string-match "node_modules" clean-name)
                     (not (file-directory-p file)))))
             (directory-files containing-dir t)))

(defun apigee--proper-files (containing-dir &optional suffix)
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

(defun apigee--sort-strings (strings)
  "lexicographically sort a list of STRINGS"
  (sort strings
        (lambda (a b) (string< a b ))))

(defun apigee--sort-by-string-car (list-o-lists)
  "sort LIST-O-LISTS, a list of lists, each of the form (STRING (LIST)),
lexicographically by the car of each element, which is a string."
  (sort list-o-lists
        (lambda (a b) (string< (car a) (car b) ))))

(defun apigee--get-target-template-contents (template-filename)
  "return the contents of the target template file.
"
  (let ((filename
         (if (s-starts-with? "/" template-filename)
             template-filename
         (apigee--join-path-elements apigee--base-template-dir "targets" template-filename))))
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun apigee--get-policy-template-contents (template-filename)
  "return the contents of the policy template file.
"
  (let ((filename
         (apigee--join-path-elements apigee--base-template-dir "policies" template-filename)))
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun apigee--templates-for-one-policy-type (policy-type-directory)
  "loads the policy templates for one POLICY-TYPE-DIRECTORY, which is a fully-qualified
directory, ending in a policy-type name eg AccessEntity.  This is used to
populate the menu of available policy templates."
  (let (templ-list)
    (dolist (this-template (apigee--proper-files policy-type-directory ".xml"))
      (push (file-name-nondirectory this-template) templ-list))
    (list (file-name-nondirectory policy-type-directory)
          (apigee--sort-strings templ-list))))

(defun apigee--policy-template-count ()
  "returns the total number of policy templates available in
`apigee--policy-template-alist'"
  (apply '+
          (mapcar (lambda (item) (length (cadr item)))
                  apigee--policy-template-alist)))

(defun apigee--load-asset-templates (sym label subdir)
  "Load templates for a kind of asset from the template dir, then set the symbol to the alist."
  (let ((top-level-dir (apigee--join-path-elements apigee--base-template-dir subdir )))
    (set sym
         (let (template-list)
           (dolist (template-name (apigee--proper-subdirs top-level-dir))
             (push
              (list (file-name-nondirectory template-name) template-name)
              template-list)
             (and apigee--verbose-logging (message "%s template %s" label template-name)))
           (reverse (apigee--sort-by-string-car template-list))))
    (list label (length (symbol-value sym)) top-level-dir)))

(defun apigee-load-proxy-templates ()
  "Load proxy templates from the proxy template dir. "
  (apigee--load-asset-templates 'apigee--proxy-template-alist "proxy" "proxies"))

(defun apigee-load-sharedflow-templates ()
  "Load templates for sharedflows from the template dir. "
  (apigee--load-asset-templates 'apigee--sharedflow-template-alist "sharedflow" "sharedflows"))

(defun apigee-load-policy-templates ()
  "Load policy templates from the policy template dir.

Each child under the template directory should have the name of a policy-type.
Within that child dir, one or more template files, each ending in .xml,
which contain templates for the policy. The name of the file will be
the name of the template in the resulting menu.
"
  (let ((top-level-dir (apigee--join-path-elements apigee--base-template-dir "policies")))
    (setq
        apigee--policy-template-alist
        (let (template-list)
          (dolist (policy-type (apigee--proper-subdirs top-level-dir))
            (push
             (apigee--templates-for-one-policy-type policy-type)
             template-list)
            (and apigee--verbose-logging (message "policy type %s" policy-type)))
          (reverse (apigee--sort-by-string-car template-list))))
    (list "policy" (apigee--policy-template-count) top-level-dir)))


(defun apigee-load-target-templates ()
  "Load target templates from the target template dir.

Each child under the template directory should be a .xml file defining a target.
The name of the file will be the name of the template in the resulting menu.
"
  (let ((top-level-dir (apigee--join-path-elements apigee--base-template-dir "targets" )))
    (setq apigee--target-template-alist
         (let (template-list)
           (dolist (template-fname (apigee--proper-files top-level-dir ".xml"))
             (push
              (list (apigee--trim-xml-suffix (file-name-nondirectory template-fname)) template-fname)
              template-list)
             (and apigee--verbose-logging (message "target template %s" template-fname)))
           (let ((v (length template-list)))
             (message "length of list %d" v))
           (reverse (apigee--sort-by-string-car template-list))))
    (list "target" (length apigee--target-template-alist) top-level-dir)))


(defun apigee-load-templates (top-level-dir &optional interactive)
  "Load templates for proxies and policies from the top level directory TOP-LEVEL-DIR.

Under TOP-LEVEL-DIR there should be sub-directories named \"policies\", \"proxies\",
and \"sharedflows\".
Within each of those, there will be templates for those entities.
"
  (interactive
   (list
    (read-directory-name
     "the template dir?: "
     apigee--base-template-dir apigee--base-template-dir t)
    t))
  (setq apigee--base-template-dir top-level-dir)
  (let ((results (list (apigee-load-proxy-templates)
                       (apigee-load-policy-templates)
                       (apigee-load-target-templates)
                       (apigee-load-sharedflow-templates)
                       )))
    (when interactive
      (message "Loaded templates: [%s] from %s"
               (mapconcat
                (lambda (x) (format "%d %s" (cadr x) (car x)))
                results
                ", ")
               top-level-dir))))

(defun apigee--trim-xml-suffix (s)
  "trims the .xml suffix from a template file name"
  (s-chop-suffix ".xml" s))

(defun apigee--get-menu-position ()
  "get the position for the popup menu"
  (if (fboundp 'posn-at-point)
      (let ((x-y (posn-x-y (posn-at-point (point)))))
        (list (list (+ (car x-y) 10)
                    (+ (cdr x-y) 20))
              (selected-window)))
    t))

(defun apigee--find-bundle (flavor) ;; "apiproxy" or "sharedflowbundle"
  "returns list of (PATH TYPE) if a bundle is found in the current directory."
  (let ((right-here (concat (file-name-directory default-directory) flavor)))
    (if (apigee--is-existing-directory right-here)
        (list (file-name-directory default-directory) flavor)
      (let ((elts (reverse (split-string (file-name-directory default-directory) "/")))
            r)
        (while (and elts (not r))
          (if (string= (car elts) flavor)
              (setq r (reverse (cdr elts)))
            (setq elts (cdr elts))))
        (if r
            (list (mapconcat 'identity r "/") flavor ))))))

(defun apigee--type-of-bundle ()
  "returns \"apiproxy\" or \"sharedflowbundle\", or nil"
  (cadr (cl-some 'apigee--find-bundle '("apiproxy" "sharedflowbundle"))))

(defun apigee--root-path-of-bundle ()
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
         (apigee--insure-trailing-slash
          (car (cl-some 'apigee--find-bundle '("apiproxy" "sharedflowbundle"))))))
    (and path (file-truename path))))

(defun apigee--target-name-is-available (tname)
  "is a target by this name available? Eg, does a file NOT exist with this name?"
  (let ((filename-to-check
         (concat (apigee--root-path-of-bundle) "apiproxy/targets/" tname ".xml")))
    (not (file-exists-p filename-to-check))))


(defun apigee--suggested-target-name (template-name)
  "suggest a name for a target given a TEMPLATE-NAME for the target.
Based on file availability."
  (let ((val 1)
        (next-name (lambda (v) (concat template-name "-" (format "%d" v)))))
      (let ((tname (funcall next-name val)))
        (while (not (apigee--target-name-is-available tname))
          (setq val (1+ val)
                tname (funcall next-name val)))
        tname)))


;;;###autoload
(defun apigee-add-target ()
  "Invoke this interactively, and the fn will prompt the user to
choose a target type to insert.
"
  (interactive)
  (let ((apiproxy-dir (apigee--root-path-of-bundle))
        (template-name
           (ido-completing-read
            (format "target template: ")
            (mapcar (lambda (x) (car x)) apigee--target-template-alist)
            nil nil nil)))
    (when template-name
      (let ((target-dir (concat apiproxy-dir "apiproxy/targets/"))
            (have-name nil)
            (target-name-prompt "target name: ")
            (choice (assoc template-name apigee--target-template-alist)))
        (let* ((template-filename (nth 1 choice))
              (raw-template (apigee--get-target-template-contents template-filename)))
          (and (not (file-exists-p target-dir))
               (make-directory target-dir))

          ;;(asset-name (read-string (format "%s name?: " asset-type) default-value nil default-value)))

            (let* ((default-value (apigee--suggested-target-name template-name))
                  (target-name
                   (let (n)
                     (while (not have-name)
                       (setq n (read-string target-name-prompt default-value nil default-value)
                             have-name (apigee--target-name-is-available n)
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


(defun apigee--generate-policy-menu ()
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
  (let ((candidates apigee--policy-template-alist))
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
              (vector (intern cat) (apigee--join-path-elements cat template))
              (cons (apigee--trim-xml-suffix template) n))
            ))

        (setq j (1+ j)))
      keymap)))


(defun apigee--prompt-user-with-policy-choices ()
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
    (x-popup-menu (apigee--get-menu-position)
                  (apigee--generate-policy-menu)))

(defun apigee--is-existing-directory (dir-name)
  "Tests to see whether a name refers to an existing directory."
  (and
   (file-exists-p dir-name)
   (let ((attrs (file-attributes dir-name)))
     (and
      (car attrs)
      (not (stringp (car attrs)))))))

(defun apigee--fixup-script-name (name &optional prefix)
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

(defun apigee--policy-name-is-available (pname)
  "Return true if the passed policy name PNAME is unused, in other words
if no file exists by that name in the given proxy.
"
  (let ((filename-to-check
         (concat (apigee--root-path-of-bundle) (apigee--type-of-bundle) "/policies/" pname ".xml")))
    (not (file-exists-p filename-to-check))))

(defun apigee--string-is-uppercase-acronym-p (str)
  (let ((case-fold-search nil))
    (string-match-p "\\`[-A-Z0-9]*\\'" str)))

(defun apigee--maybe-capitalize (str)
  "Capitalizes the string STR unless the string is an acronym like HS256 or PBES2."
  (if (apigee--string-is-uppercase-acronym-p str) str (s-capitalize str)))

(defun apigee--suggested-policy-name (ptype filename)
  "Returns a string that contains a default policy name. Derives the name
from PTYPE policy type, and FILENAME which is the fully-qualified filename.
Uses a counter that is indexed per policy type within each API Proxy.
"
  (let* ((basename (file-name-sans-extension (file-name-nondirectory filename)))
         (name-elements (split-string basename " "))
         (is-jwt (or (string= ptype "JWT") (string= ptype "JWS")))
         (name-suffix (mapconcat 'apigee--maybe-capitalize
                                 (if is-jwt (seq-drop name-elements 1) name-elements)
                                 "-"))
         (ptype (if is-jwt
                    (concat (s-capitalize (car name-elements)) ptype)
                  (or (cadr (assoc ptype apigee--policytype-shortform)) ptype))))
    (let ((val 0)
          (next-name (lambda (v)
                       (if (> v 0)
                           (concat ptype "-" name-suffix "-" (format "%d" v))
                         (concat ptype "-" name-suffix)))))
      (let ((pname (funcall next-name val)))
        (while (not (apigee--policy-name-is-available pname))
          (setq val (1+ val)
                pname (funcall next-name val)))
        pname))))


;;;###autoload
(defun apigee-add-policy ()
  "Invoke this interactively, and the fn will prompt the user to
choose a policy type to insert. It will then ask for a name for
the policy, create the appropriate XML file, and using
yas-snippet, expand the template associated to the chosen policy,
into the policy file. It then will open any resource files as
appropriate.

"
  (interactive)
  (let ((bundle-dir (apigee--root-path-of-bundle))
        (bundle-type (apigee--type-of-bundle))
        (choice (apigee--prompt-user-with-policy-choices)))
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
            (let* ((raw-template (apigee--get-policy-template-contents template-filename))
                   (default-value (apigee--suggested-policy-name ptype template-filename))
                   (policy-name
                    (let (n)
                      (while (not have-name)
                        (setq n (read-string policy-name-prompt default-value nil default-value)
                              have-name (apigee--policy-name-is-available n)
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
               (concat "        <Step>\n          <Name>" policy-name "</Name>\n        </Step>\n"))
              (message "yank to add the step declaration...")
              )))))))

(defun apigee--is-policy-file (s)
  "returns true if the filename represents a policy file in the current
proxy/sharedflow bundle"
  (and
   (s-starts-with? (concat (apigee--root-path-of-bundle) (apigee--type-of-bundle) "/policies/") s)
   (file-exists-p s)))

(defun apigee--current-policy-name ()
  "returns the name of the current policy, if the current buffer
is visiting a policy file. otherwise nil."
  (let ((current-filename (buffer-file-name)))
    (if (apigee--is-policy-file current-filename)
       (s-chop-suffix ".xml" (car (reverse (split-string current-filename
                                                         "/")))))))
(defun apigee--list-policies ()
  "returns a list of policies in the current bundle"
  (if (apigee--root-path-of-bundle)
      (let* ((policy-dir
             (concat (apigee--root-path-of-bundle) (apigee--type-of-bundle) "/policies/"))
            (fq-names (apigee--sort-strings (apigee--proper-files policy-dir ".xml"))))
        (mapcar (lambda (s) (s-chop-suffix ".xml" (car (reverse (split-string s "/")))))
                fq-names))))

(defun apigee-rename-policy ()
  "rename a policy to a new name"
  (interactive)
  (let ((current-policies (apigee--list-policies))
        (predicate-ignored nil)
        (require-match nil)
        (initial-input (apigee--current-policy-name))
        (hist nil)
        (def (apigee--current-policy-name)))
    (let* ((name-of-policy-to-rename
            (ido-completing-read "rename policy: " current-policies
                                 predicate-ignored require-match initial-input hist def))
           ;; todo: replace this. Don't need a completing read here. Just a read.
           (new-name (read-string (format "rename '%s' to : " name-of-policy-to-rename)
                                  name-of-policy-to-rename nil name-of-policy-to-rename)))

      (if (s-equals? name-of-policy-to-rename new-name)
          (message "nothing to do.")
        (if (member new-name current-policies)
            (message "there is already a policy with that name.")
          (let* ((bundle-dir (concat (apigee--root-path-of-bundle) (apigee--type-of-bundle)))
                 (policy-dir (concat bundle-dir "/policies/")))
            (let ((fq-old-fname (concat policy-dir name-of-policy-to-rename ".xml"))
                  (fq-new-fname (concat policy-dir new-name ".xml")))
              (message "renaming %s to %s..." name-of-policy-to-rename new-name)

              (let ((re1 (concat "\\<" (regexp-quote name-of-policy-to-rename) "\\>"))
                    (interactive-buffer-name (buffer-name))
                    (buf-for-policy-file (get-file-buffer fq-old-fname))
                    (count-of-changes 0)
                    (process-one-file (lambda (one-file)
                                        (with-current-buffer (find-file-noselect one-file)
                                          (save-excursion
                                            (goto-char (point-min))
                                            (let ((made-at-least-one-change nil)
                                                  (original-modified (buffer-modified-p)))
                                              (while (re-search-forward re1 nil t)
                                                (replace-match new-name)
                                                (setq made-at-least-one-change t))

                                              (if made-at-least-one-change
                                                  (setq count-of-changes (1+ count-of-changes)))

                                              (if (and made-at-least-one-change
                                                       (not original-modified)
                                                       (not (s-equals-p interactive-buffer-name (buffer-name))))
                                                  (save-buffer))))))))

                ;; rename the file for the policy
                (rename-file fq-old-fname fq-new-fname)

                ;; rename the buffer for the policy, if it is open
                (if buf-for-policy-file
                    (with-current-buffer buf-for-policy-file
                      (rename-buffer new-name t) ;; get unique buffer name
                      (set-visited-file-name fq-new-fname) ;; sets modified flag t
                      (set-buffer-modified-p nil)))

                ;; modify the policy file itself
                (funcall process-one-file fq-new-fname)

                ;; modify all references to this policy
                (dolist (dir
                         (mapcar (lambda (s) (concat bundle-dir "/" s))
                                 (list "targets" "proxies" "sharedflows")))
                  (if (file-exists-p dir)
                      (dolist (one-file (apigee--proper-files dir ".xml"))
                        ;; edit the file to replace
                        (funcall process-one-file one-file))))
                (message "made %d changes" count-of-changes)))))))))


;; (defun apigee-maybe-sync-policy-filename ()
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

(defun apigee--cleanup-newlines ()
  "collapse multiple newlines"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(\n\s*\\)\\([\s]*\n\\)" (point-max) t)
      (replace-match (match-string 1)))))

(defun apigee--cleanup-quotes ()
  "replace escaped quote with quote character"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([ =]\\)&quot;" (point-max) t)
      (replace-match (concat (match-string 1) "\"")))
    (goto-char (point-min))
    (while (re-search-forward "&quot;\\([ <)]\\)" (point-max) t)
      (replace-match (concat "\"" (match-string 1))))))

(defun apigee--serialize-xml-elt (elt)
  "Serialize an XML element into the current buffer, and then collapse newlines."
  (erase-buffer)
  (xml-print elt)
  (apigee--cleanup-newlines)
  (apigee--cleanup-quotes)
  (save-excursion
    (indent-region (point-min) (point-max)))
  )

(defun apigee--verify-exactly-one (xml-file-list source-dir)
  "verifies that there is exactly one xml file."
      (if (not xml-file-list)
          (error
           (message "[apigee] cannot find XML file in %s, in `apigee--verify-exactly-one'" source-dir)))
      (if (not (= (length xml-file-list) 1))
          (error
           (message "[apigee] found more than one XML file in %s, in `apigee--verify-exactly-one'" source-dir))))

(defun apigee--copy-subdirs (subdirs source-dir dest-dir)
  (while subdirs
        (let* ((this-dir (car subdirs))
               (source-subdir (apigee--join-path-elements source-dir this-dir))
               (dest-subdir (apigee--join-path-elements dest-dir this-dir)))
          (if (apigee--is-existing-directory source-subdir)
              (copy-directory source-subdir dest-subdir t nil)))
        (setq subdirs (cdr subdirs))))

(defun apigee--copy-proxy-template-files (proxy-name source destination)
  "copy files from the SOURCE template directory to the DESTINATION directory,
changing names and replacing / expanding things as appropriate."
  (let ((source-apiproxy-dir (apigee--join-path-elements source "apiproxy"))
        (dest-apiproxy-dir (apigee--join-path-elements destination "apiproxy")))

    (let ((xml-file-list (apigee--proper-files source-apiproxy-dir ".xml")))
      (apigee--verify-exactly-one xml-file-list source-apiproxy-dir)
      ;; create the toplevel destination directory
      (make-directory dest-apiproxy-dir t)
      ;; copy, then intelligently modify the toplevel proxy bundle definition file
      (let ((new-xml-file-name (apigee--join-path-elements dest-apiproxy-dir (concat proxy-name ".xml"))))
        (copy-file (car xml-file-list) new-xml-file-name nil t t nil)
        (with-current-buffer (find-file-noselect new-xml-file-name)
          (let*
              ((root (xml-parse-region (point-min) (point-max)))
               (apiproxy-elt (car root))
               (apiproxy-elt-attrs (xml-node-attributes apiproxy-elt))
               (display-name-elt (car (xml-get-children apiproxy-elt 'DisplayName))))
            (setcdr (assq 'name apiproxy-elt-attrs) proxy-name)
            (setcar (cddr display-name-elt) proxy-name)
            (apigee--serialize-xml-elt root))
          (save-buffer))))

    (apigee--copy-subdirs (list "proxies" "policies" "resources" "targets") source-apiproxy-dir dest-apiproxy-dir)

    ;; if we can find a single XML file in the proxies dir, modify it
    (let* ((dest-proxyendpoints-dir (apigee--join-path-elements dest-apiproxy-dir "proxies"))
           (xml-file-list (apigee--proper-files dest-proxyendpoints-dir ".xml")))
      (if (not xml-file-list)
          (error
           (message "[apigee] cannot find XML file in %s, in `apigee--copy-proxy-template-files'" dest-proxyendpoints-dir)))
      (if (= (length xml-file-list) 1)
          (with-current-buffer (find-file-noselect (car xml-file-list))
            (let*
                ((root (xml-parse-region (point-min) (point-max)))
                 (proxy-endpoint-elt (car root))
                 (http-proxy-conn-elt (car (xml-get-children proxy-endpoint-elt 'HTTPProxyConnection)))
                 (basepath-elt (car (xml-get-children http-proxy-conn-elt 'BasePath))))
              (setcar (cddr basepath-elt) (concat "/" proxy-name))
              (apigee--serialize-xml-elt root)
              (apigee--cleanup-quotes))
            (save-buffer 0))))
    ))

(defun apigee--copy-sharedflow-template-files (sf-name source destination)
  "copy files from the SOURCE template directory to the DESTINATION directory,
changing names and replacing / expanding things as appropriate."
  (let ((source-sf-dir (apigee--join-path-elements source "sharedflowbundle"))
        (dest-sf-dir (apigee--join-path-elements destination "sharedflowbundle")))
    (let ((xml-file-list (apigee--proper-files source-sf-dir ".xml")))
      (apigee--verify-exactly-one xml-file-list source-sf-dir)
      (make-directory dest-sf-dir t)
      ;; copy, then intelligently modify the toplevel proxy bundle definition file
      (let ((new-xml-file-name (apigee--join-path-elements dest-sf-dir (concat sf-name ".xml"))))
        (copy-file (car xml-file-list) new-xml-file-name nil t t nil)
        (with-current-buffer (find-file-noselect new-xml-file-name)
          (let*
              ((root (xml-parse-region (point-min) (point-max)))
               (sfbundle-elt (car root))
               (sfbundle-elt-attrs (xml-node-attributes sfbundle-elt))
               (display-name-elt (car (xml-get-children sfbundle-elt 'DisplayName))))
            (setcdr (assq 'name sfbundle-elt-attrs) sf-name)
            (setcar (cddr display-name-elt) sf-name)
            (apigee--serialize-xml-elt root))
          (save-buffer))))
    (apigee--copy-subdirs (list "sharedflows" "policies") source-sf-dir dest-sf-dir)))

(defun apigee--new-asset-from-template (asset-type asset-name template-alist template-name containing-dir)
  "Non-interactive function to create a new asset from a template.
ASSET-TYPE - either \"proxy\" or \"sharedflow\".
ASSET-NAME - name of the to-be-created asset.
TEMPLATE-ALIST - the alist of templates.
TEMPLATE-NAME - the name of the asset template, as stored in the templates/<asset-type> directory.
CONTAINING-DIR - name of an existing directory into which to insert the new asset.
"
  (if (not (apigee--is-existing-directory containing-dir))
    (error
     (message "[apigee] containing-dir does not exist in `apigee--new-asset-from-template'")))
  (let ((template-match (assoc template-name template-alist))
        (new-dir (apigee--join-path-elements containing-dir asset-name)))
    (if (apigee--is-existing-directory new-dir)
        (error
         (message "[apigee] new-dir already exists in `apigee--new-asset-from-template'")))
    (if (not template-match)
        (error
         (message "[apigee] unknown template in `apigee--new-asset-from-template'")))
    (let ((template-dir (cadr template-match))
          (copy-fn (intern (format "apigee--copy-%s-template-files" asset-type))))
      (funcall copy-fn asset-name template-dir new-dir))
    (find-file-existing new-dir)))

(defun apigee--fresh-recent-asset-homes (with-time)
  "returns the list of recently used asset homes, eliminating stale entries
and non-existent directories, in sorted order.  If WITH-TIME is non-nil, emit
the list with time (suitable for persisting). Otherwise without - it will be a
bare list of strings representing directory paths, suitable for use within an
`ido-completing-read'."
  (let ((result
         (let ((days-considered-stale 30))
           (-sort (lambda (a b)
                    (> (float-time (time-subtract (apply 'encode-time (cadr a)) (apply 'encode-time (cadr b)))) 0))
                  (-filter (lambda (x)
                             (and
                              (apigee--is-existing-directory (car x))
                              (apigee--time-is-within-days (apply 'encode-time (cadr x)) days-considered-stale)))
                           apigee--recently-used-asset-homes)))))
    (if with-time result
      (-map (lambda (n) (car n)) result))))

(defun apigee--time-is-within-days (the-time delta-days)
  "Returns t if THE-TIME is less than DELTA-DAYS ago."
  (let ((delta-seconds (* 24 delta-days 60 60)))
    (< (float-time (time-subtract (current-time) the-time)) delta-seconds)))

(defun apigee--prompt-for-containing-dir ()
  "prompt user for a containing directory, and return it. Create the directory
if necessary."
  (let ((homedir (concat (getenv "HOME") "/"))
        (candidate-list (cons default-directory (apigee--fresh-recent-asset-homes nil))))
    (let ((containing-dir
           (ido-completing-read
            "containing directory?: "
            (mapcar (lambda (x) (replace-regexp-in-string homedir "~/" x))
                    (delq nil (delete-dups candidate-list))) nil nil nil)))
          (and (not (file-exists-p containing-dir))
               (make-directory containing-dir t))
          containing-dir)))


(defun apigee-new-proxy (arg)
  "Interactive fn that creates a new exploded proxy bundle directory
structure. Prompts for the name of the API Proxy, and the base template
for the proxy.

When invoked with a prefix, this fn will prompt the user also for
the name of the directory in which to store the apiproxy.

When invoked without a prefix, it uses the most recent asset home
directory. If no directory has ever been used, it prompts for the directory.

"
  (interactive "P")
  (apigee--new-asset "proxy" arg))


(defun apigee-new-sharedflow (arg)
  "Interactive fn that creates a new exploded sharedflow bundle directory
structure. Prompts for the name of the sharedflow, and the base template
for the thing.

When invoked with a prefix, this fn will prompt the user also for
the name of the directory in which to store the sharedflow.

When invoked without a prefix, it uses the most recent asset home
directory. If no directory has ever been used, it prompts for the directory.

"
  (interactive "P")
  (apigee--new-asset "sharedflow" arg))


(defun apigee--remember-asset-home (containing-dir)
  "stores one directory with the current time in the list"
  (setq apigee--recently-used-asset-homes
        (-remove (lambda (x)  (equal (car x) containing-dir)) apigee--recently-used-asset-homes))
  (setq apigee--recently-used-asset-homes (cons (list containing-dir (decode-time nil t))
                                              apigee--recently-used-asset-homes)))

(defun apigee--new-asset (asset-type prompt-arg)
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
  (let ((alist-sym (intern (format "apigee--%s-template-alist" asset-type))))
    (let ((asset-name (read-string (format "%s name?: " asset-type) nil nil nil))
          (template
           (ido-completing-read
            (format "%s template: " asset-type)
            (mapcar (lambda (x) (car x)) (symbol-value alist-sym))
            nil nil nil))
          (containing-dir
           (if prompt-arg
               (apigee--prompt-for-containing-dir)
             (or (car (apigee--fresh-recent-asset-homes nil)) (apigee--prompt-for-containing-dir)))))
      (apigee--remember-asset-home containing-dir)
      (apigee--new-asset-from-template asset-type asset-name (symbol-value alist-sym) template containing-dir))))


;; restore last known state, and set a timer to persist state periodically
(eval-after-load "apigee"
  '(progn
     (apigee--restore-state)
     (if (not apigee--base-template-dir)
         (setq apigee--base-template-dir (concat (file-name-directory apigee--load-file-name) "templates/")))
     (if apigee--base-template-dir
         (apigee-load-templates apigee--base-template-dir))
     (setq apigee-timer
           (run-with-timer (* 60 apigee--timer-minutes) (* 60 apigee--timer-minutes) 'apigee--persist-state))))

(provide 'apigee)

;;; apigee.el ends here
