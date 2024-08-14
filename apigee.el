;;; apigee.el --- utility functions for working with Apigee platform in emacs
;;
;; Copyright (C) 2017-2023 Dino Chiesa and Google, LLC.
;;
;; Author     : Dino Chiesa
;; Maintainer : Dino Chiesa <dchiesa@google.com>
;; Created    : May 2017
;; Modified   : December 2023
;; Version    : 1.2
;; Keywords   : apigee
;; Requires   : s.el, xml.el
;; License    : Apache 2.0
;; X-URL      : https://github.com/DinoChiesa/apigee-el
;; Last-saved : <2024-August-14 09:47:24>
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
;;    Copyright 2017-2023 Google LLC.
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
(require 'seq)
(require 'popup)
(require 'xml)
(require 'xml-to-string)
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

(defvar apigee-commands-alist
  '(
   (import . "%apigeecli apis create bundle -f apiproxy --name %n -o %o --token %t")
   (deploy . "%apigeecli apis deploy --wait --name %n --ovr --org %o --env %e --token %t")
   (import-and-deploy . "%apigeecli apis create bundle -f apiproxy --name %n -o %o --token %t ; %apigeecli apis deploy --wait --name %n --ovr --org %o --env %e --token %t")
   (lint .  "%apigeelint -s . -e TD002,TD007 -f visualstudio.js")
   ))

(defvar apigee-programs-alist
  '(
    (gcloud . "~/path/to/gcloud")
    (apigeecli . "~/path/to/apigeecli")
    (apigeelint . "node ~/path/to/apigeelint/cli.js")
    )
  "users should set these in their emacs init file, via, for example,
  (setf (alist-get 'apigeecli apigee-programs-alist)
           \"~/.apigeecli/bin/apigeecli\")"
  )

(defvar apigee-placeholders-alist
  '(
    (n . proxy-name)
    (o . (apigee-get-organization want-prompt))
    (e . (apigee-get-environment want-prompt))
    (t . (apigee-gcloud-auth-print-access-token))
    )
  )

(add-to-list 'compilation-error-regexp-alist 'apigeelint-visualstudio-error-with-lines)
(add-to-list 'compilation-error-regexp-alist 'apigeelint-visualstudio-error-no-line)

(add-to-list 'compilation-error-regexp-alist-alist
             '(apigeelint-visualstudio-error-with-lines
               "^\\(\\([^ \n]+\\)(\\([0-9]+\\),\\([0-9]+\\))\\): \\(.+\\)$"
               2 3 4 2 1 ))

;; to modify:
;; (setf
;;  (alist-get 'apigeelint-visualstudio-error-with-lines compilation-error-regexp-alist-alist)
;;    '("^\\(\\([^ \n]+\\)(\\([0-9]+\\),\\([0-9]+\\))\\): \\(.+\\)$"
;;     2 3 4 2 1 ))


(add-to-list 'compilation-error-regexp-alist-alist
             '(apigeelint-visualstudio-error-no-line
               "^\\(\\([^ \n]+\\)(\\(0\\)\\)): \\(.+\\)$"
               2 3 nil 2 1 ))

;; to modify:
;; (setf
;;  (alist-get 'apigeelint-visualstudio-error-no-line compilation-error-regexp-alist-alist)
;;    '( "^\\(\\([^ \n]+\\)(\\(0\\)\\)): \\(.+\\)$"
;;     2 3 nil 2 1 ))


(defvar apigee-organization "your-organization-here"
  "The organization to use for deployments.")
(defvar apigee-environment "your-environment-here"
  "The environment to use for deployments.")

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
   '("xml" "<AboutMe>
  <Emplid>123</Emplid>
  <Languages>German(Speaking)</Languages>
  <Languages>German(Writing)</Languages>
  <Languages>German(Reading)</Languages>
  <Languages>French(Reading)</Languages>
  <Languages>French(Speaking)</Languages>
  <Languages>French(Writing)</Languages>
  <Languages>English(Reading)</Languages>
  <Languages>English(Writing)</Languages>
  <Languages>English(Speaking)</Languages>
</AboutMe>")
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
    (if (file-exists-p dat-file-path)
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
                      (set (intern setting-name) (cadr one-setting))))))))
      )))

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
  (seq-remove (lambda (file)
                (let ((clean-name (file-name-nondirectory file)))
                  (or (string-match "^\\." clean-name)
                      (string-match "node_modules" clean-name)
                      (not (file-directory-p file)))))
              (directory-files containing-dir t)))

(defun apigee--proper-files (containing-dir &optional suffix)
  "Return list of full paths of proper files found in CONTAINING-DIR.
Optionally filters on files with the given extension or SUFFIX."
  (seq-remove (lambda (file)
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

(defun apigee--get-target-template-filename (template-filename)
  "return the fullpath name of the target template file.
"
  (if (s-starts-with? "/" template-filename)
      template-filename
    (apigee--join-path-elements apigee--base-template-dir "targets" template-filename)))


(defun apigee--get-template-contents (fullpath-template-filename)
  "return the contents of the template file."
  (with-temp-buffer
    (insert-file-contents fullpath-template-filename)
    (buffer-substring-no-properties (point-min) (point-max))))

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

The apigee--policy-template-alist gets set with something like this:

  ((\"AccessEntity\"
    (\"app.xml\" \"basic.xml\" \"developer.xml\"))
   (\"AssignMessage\"
    (\"Set Content-Type.xml\" \"Store Original header.xml\"))
   (\"BasicAuthentication\"
    (\"Decode Inbound.xml\" \"Encode Outbound.xml\"))
   ...)


Each item in the list is a list, (POLICY-TYPE (TEMPLATE...)),
where POLICY-TYPE is one of {Quota, XMLToJSON, Javascript, etc},
and TEMPLATE is the shortname of the file containing a template
to fill in for a new policy.

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

(defun apigee-get-organization (&optional want-prompt)
  (interactive "P")
  (let
      ((org-local
        (if
            (or want-prompt
                (not apigee-organization))
            (read-string "organization name: ")
          apigee-organization)))
    (setq apigee-organization org-local)))

(defun apigee-get-environment (&optional want-prompt)
  (interactive "P")
  (let
      ((env-local
        (if
            (or want-prompt
                (not apigee-environment))
            (read-string "environment name: ")
          apigee-environment)))
    (setq apigee-environment env-local)))




;; (if
;;     (apigee--prompt-for-containing-dir)
;;            (or (car (apigee--fresh-recent-asset-homes nil)) (apigee--prompt-for-containing-dir)))))
;;
;;  (let* ((org-local
;;          (or apigee-organization
;;              (read-string "organization name: ")))
;;         (env-local
;;          (or apigee-environment
;;              (read-string "environment name: ")))
;;         )
;;    (list org-local env-local)))
;;
;;    (if (and org env)
;;        (progn
;;          (setq apigee-organization org
;;                apigee-environment env)
;;          )
;;      )
;;    )



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
               (source-target-template-filename
                (apigee--get-target-template-filename template-filename)))
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
                 (elaborated-contents
                  (apigee--elaborate-template
                   ;; does this need to be qualified as fullpath?
                   source-target-template-filename target-name)))

            ;; create the file, expand the snippet, save it.
            (find-file (concat target-dir target-name ".xml"))
            ;; yas-expand-snippet-sync does not return until the snip is expanded.
            (yas-expand-snippet-sync elaborated-contents (point) (point))
            (save-buffer)
            ;; here, optionally open the resource file, if any
            (kill-new
             (concat "<RouteRule name='foo'><Target>" target-name "</Target></RouteRule>"))
            (message "yank to add the RouteRule declaration...")
            ))))))


(defun apigee--generate-policy-menu ()
  "From the filesystem of policy templates, generate a keymap suitable for
use as a menu in `x-popup-menu' .

The output is a list of the form (TITLE PANE1 PANE2...), where
each pane is a list of form (TITLE ITEM1 ITEM2...).  Each ITEM is
a cons cell (STRING . VALUE), where the VALUE is a cons cell
containing the policy type and template name.

The list looks like this:

  (\"Undisplayed Title\"
   (\"AccessControl\"
    (\"basic\" \"AccessControl\" . \"basic.xml\"))
   (\"BasicAuthentication\"
    (\"Encode Outbound\" \"BasicAuthentication\" . \"Encode Outbound.xml\")
    (\"Decode Inbound\" \"BasicAuthentication\" . \"Decode Inbound.xml\"))
    ...
   (\"XSL\"
    (\"basic.xml\" \"XSL\" . \"basic.xml\")))


The intention is to provide input to `x-popup-menu', to display a cascading,
multi-level popup menu.

When a selection is made, the result is the VALUE.

"
  (let ((candidates
         (seq-sort-by #'car #'string< (copy-sequence apigee--policy-template-alist))))

    (let ((sort-by-name (lambda (a b) (not (string< (downcase a) (downcase b))))))
      (cons "Undisplayed Title"
            (mapcar (lambda (template-set)
                      (let* ((policy-type (car template-set))
                             (templates (sort (copy-sequence (cadr template-set)) sort-by-name)))
                        (cons policy-type
                              (mapcar
                               (lambda (item) (cons
                                               (apigee--trim-xml-suffix item) ;; shortname
                                               (cons policy-type item)))
                               templates))
                        ))
                    candidates))))
  )


;; (defun apigee--convert-policy-alist-to-popup-menu-spec ()
;;   "convert apigee--policy-template-alist like
;; ((\"XSL\"
;;   (\"basic.xml\"))
;;  (\"XMLToJSON\"
;;   (\"full options.xml\" \"strip and treat as array.xml\"))
;;   ...
;;
;; into a spec for popup-cascade-menu "
;;
;;   (let ((sorted-list
;;          (seq-sort-by #'car #'string< (copy-sequence apigee--policy-template-alist))))
;;     (mapcar (lambda (category-and-templates)
;;               (let ((category (car category-and-templates))
;;                     (templates (cadr category-and-templates)))
;;               (cons
;;                category
;;                (mapcar
;;                 (lambda (tmpl)
;;                   (popup-make-item
;;                    (apigee--trim-xml-suffix tmpl)
;;                    ;; :summary
;;                    ;; (apigee--trim-xml-suffix tmpl)
;;                    :value
;;                    (list (intern category)
;;                          (apigee--join-path-elements category tmpl))))
;;                 templates))))
;;             sorted-list)))

(defun apigee--prompt-user-with-policy-choices ()
  "Prompt the user with the available choices.
In this context the available choices is the hierarchical list
of available policies.
"
  (interactive)
  ;; 20220427-1042
  ;; x-popup-menu crashes on emacs 28.1
  ;; or, maybe it was because I was using it incorrectly.
  ;; as of 20231218-1424, it seems to be working correctly.
  (x-popup-menu (apigee--get-menu-position)
                (apigee--generate-policy-menu))



  ;; popup-cascade-menu works, sort of.  The cascading is sort of messy.  That's
  ;; just an effect of how the length of the prefix is constructed in
  ;; popup-create. I couldn't figure it out in popup.el.

  ;; Can customize the face with:
  ;;(set-face-attribute 'popup-menu-face nil :height 105)
  ;;(set-face-attribute 'popup-menu-selection-face nil :height 105)

  ;; But ... this will apply to all popups. Anything that uses popup.el. ??

  ;; Omitting the height param... allows a better view of the toplevel menu,
  ;; which is nice. But selecting from that level results in an error, "args out
  ;; of range" in fn `poup-line-overlay'.  I was not able to diagnose that.
  ;; So I keep the height and ... suffer with poor UI?

  ;; (popup-cascade-menu (apigee--convert-policy-alist-to-popup-menu-spec)
  ;;                     ;;:height (length apigee--policy-template-alist)
  ;;                     :height 10
  ;;                     :margin-left 2
  ;;                     :max-width 21
  ;;                     :margin-right 1
  ;;                     :around t
  ;;                     :scroll-bar t
  ;;                     :isearch t
  ;;                     )


  )

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

(defun apigee--policy-name-elements (ptype policy-template-filename)
  "Returns a list containing name elements, (PREFIX SUFFIX), for a policy file."
  (let* ((basename (file-name-sans-extension (file-name-nondirectory policy-template-filename)))
         (name-elements (split-string basename " "))
         (is-jwt (or (string= ptype "JWT") (string= ptype "JWS")))
         (name-suffix (mapconcat 'apigee--maybe-capitalize
                                 (if is-jwt (seq-drop name-elements 1) name-elements)
                                 "-"))
         (name-prefix (if is-jwt
                          (concat (s-capitalize (car name-elements)) ptype)
                        (or (cadr (assoc ptype apigee--policytype-shortform)) ptype))))
    (list name-prefix name-suffix)))

(defun apigee--suggested-policy-name (ptype filename)
  "Returns a string that contains a default policy name. Derives the name
from PTYPE policy type, and FILENAME which is the fully-qualified filename.
Uses a counter that is indexed per policy type within each API Proxy.
"
  (let* ((name-elements (apigee--policy-name-elements ptype filename))
         (name-prefix (car name-elements))
         (name-suffix (cadr name-elements)))
    (let ((val 0)
          (next-name (lambda (v)
                       (if (> v 0)
                           (concat name-prefix "-" name-suffix "-" (format "%d" v))
                         (concat name-prefix "-" name-suffix)))))
      (let ((pname (funcall next-name val)))
        (while (not (apigee--policy-name-is-available pname))
          (setq val (1+ val)
                pname (funcall next-name val)))
        pname))))

(defun apigee-gcloud-auth-print-access-token ()
  "return output of $(gcloud auth print-access-token)"
  (let* ((gcloud-pgm
          (alist-get 'gcloud apigee-programs-alist))
         (command-string (concat gcloud-pgm " auth print-access-token"))
         (output (replace-regexp-in-string "\n$" "" (shell-command-to-string command-string)))
         (lines (split-string output "\n")))
    (car (last lines))))

(defun apigee--proxy-name ()
  "returns the short name for an API proxy"
  (let ((bundle-dir (apigee--root-path-of-bundle))
        (bundle-type (apigee--type-of-bundle)))
    (if (s-equals? bundle-type "apiproxy")
        (let* ((bundle-apiproxy-dir (concat bundle-dir bundle-type))
               (file-list (apigee--proper-files bundle-apiproxy-dir ".xml"))
               (proxy-defn-file (apigee--verify-exactly-one file-list bundle-apiproxy-dir)))
          (apigee--trim-xml-suffix (file-name-nondirectory proxy-defn-file))))))

(defun apigee--get-command (symbol want-prompt)
  "get the apigeelint command for the current API proxy."
  (let ((cmd (alist-get symbol apigee-commands-alist)))
    (when cmd
      (let ((bundle-dir (apigee--root-path-of-bundle))
            (bundle-type (apigee--type-of-bundle))
            (proxy-name (apigee--proxy-name))
            (fn1 (lambda (acc cur)
                   (let* ((key (car cur))
                          (replaceable (concat "%" (symbol-name key))))
                     (if (s-contains? replaceable acc)
                         (replace-regexp-in-string replaceable (cdr cur) acc)
                       acc))))
            (fn2 (lambda (acc cur)
                   (let* ((key (car cur))
                          (replaceable (concat "%" (symbol-name key)))
                          (val (cdr cur)))
                     (if (s-contains? replaceable acc)
                         (replace-regexp-in-string replaceable (eval val t) acc)
                       acc)))))

        (setq cmd
              (seq-reduce
               fn1
               apigee-programs-alist
               cmd))

        ;; I had trouble using this seq-reduce, when using edebug-defun.
        ;; But in my experience, when NOT using the debugger, it works as intended.
        ;; Womp womp.
        (seq-reduce fn2 apigee-placeholders-alist cmd)))))

(defun apigee--run-command-for-proxy (label command-symbol &optional want-prompt)
  "Run a command for the current API proxy."
  (let ((bundle-dir (apigee--root-path-of-bundle))
        (proxy-name (apigee--proxy-name))
        (cmd (apigee--get-command command-symbol want-prompt)))
    (let ((name-function (lambda (mode) (concat "*" label " - " proxy-name "*")))
          highlight-regexp)
      (when cmd
        (let ((buf
               (compilation-start (concat "cd " bundle-dir "; " cmd) t name-function highlight-regexp)))
          (with-current-buffer buf
            ;; obscure any token that appears
            (save-excursion
              (beginning-of-buffer)
              (while (re-search-forward "--token \\([^ \n]+\\)" nil t)
                (replace-match "--token ***masked***")))
            (setq buffer-read-only t)))))))

(defun apigee-lint-asset ()
  "Run apigeelint on the API proxy."
  (interactive)
  (apigee--run-command-for-proxy "apigeelint" 'lint))

(defun apigee-import-and-deploy-proxy (have-prefix)
  "Import AND deploy the API proxy."
  (interactive "P") ;; if have-prefix is non-nil, prompt the user for org/env
  (apigee--run-command-for-proxy "apigeecli" 'import-and-deploy have-prefix))

(defun apigee-import-proxy (have-prefix)
  "Import the API proxy."
  (interactive "P") ;; if have-prefix is non-nil, prompt the user for org/env
  (apigee--run-command-for-proxy "apigeecli" 'import have-prefix))

(defun apigee-deploy-proxy (have-prefix)
  "Deploy the API proxy. This doesn't use the proxy configuration in the
filesystem, beyond the name. It's assumed you've recently called
`apigee-import-proxy', and there is a revision to deploy."
  (interactive "P") ;; if have-prefix is non-nil, prompt the user for org/env
  (apigee--run-command-for-proxy "apigeecli" 'deploy have-prefix))

(defun apigee-inject-proxy-revision-logic ()
  "Inject the policies and flow to insert a proxy revision header
into the response. Handy when modifying an existing proxy that
does not send back a revision header, or when creating a proxy
from a template that doesn't send back a revision header."
  (interactive)
  (let ((bundle-dir (apigee--root-path-of-bundle))
        (bundle-type (apigee--type-of-bundle))
        (policy-names))
    (if (s-equals? bundle-type "apiproxy")
        (let ((bundle-policy-dir (concat bundle-dir bundle-type "/policies/")))
          (dolist (am-template-file
                   (list "clean request headers from response.xml"
                         "inject proxy revision header.xml"))
            (let* ((name-elements
                    (apigee--policy-name-elements "AssignMessage" am-template-file))
                   (name-prefix (car name-elements))
                   (name-suffix (cadr name-elements))
                   (policy-name (concat name-prefix "-" name-suffix))
                   (fullpath-dest-policy-path
                    (apigee--join-path-elements bundle-policy-dir (concat policy-name ".xml"))))
              (setq policy-names (cons policy-name policy-names))
              (if (not (file-exists-p fullpath-dest-policy-path))
                  (let ((elaborated-contents
                         (apigee--elaborate-template
                          (apigee--join-path-elements apigee--base-template-dir "policies" "AssignMessage" am-template-file)
                          policy-name)))
                    ;; create the file, expand the snippet, save it.
                    (with-current-buffer (find-file-noselect fullpath-dest-policy-path)
                      ;; yas-expand-snippet-sync does not return until the snip is expanded.
                      (yas-expand-snippet-sync elaborated-contents (point) (point))
                      (save-buffer)
                      )))))

          ;; and now inject the references to these policies in the
          ;; right places in the various proxy endpoint(s).

          (let ((bundle-proxy-dir
                 (concat bundle-dir bundle-type "/proxies/")))
            (dolist (proxyendpoint-file
                     (apigee--proper-files bundle-proxy-dir ".xml"))
              (save-excursion
                (with-current-buffer (find-file-noselect proxyendpoint-file)
                  (let*
                      ((root (xml-parse-region (point-min) (point-max)))
                       (proxy-endpoint-elt (car root))
                       (default-faultrule-elt (car (xml-get-children proxy-endpoint-elt 'DefaultFaultRule)))
                       (preflow-elt (car (xml-get-children proxy-endpoint-elt 'PreFlow)))
                       (postflow-elt (car (xml-get-children proxy-endpoint-elt 'PostFlow)))
                       (step-references
                        (mapcar (lambda (s) (car (apigee--parse-xml (concat "  <Step>\n    <Name>" s  "</Name>\n  </Step>"))))
                                policy-names)))

                    (if (not preflow-elt)
                        ;; insert PreFlow node, try one of various locations.
                        (let ((position-index
                               (-some (lambda (sym) (apigee--xml-locate-first-child-node proxy-endpoint-elt sym))
                                        '(PostFlow PostClientFlow Flows DefaultFaultRule FaultRules)))
                              (new-preflow-elt
                               (car (apigee--parse-xml "<PreFlow><Request/><Response/></PreFlow>"))))
                          ;; insert before the selected element
                          (push new-preflow-elt (cdr (nthcdr position-index proxy-endpoint-elt)))
                          (setq preflow-elt (car (xml-get-children proxy-endpoint-elt 'PreFlow)))))

                    (if (not postflow-elt)
                        ;; insert PostFlow node, try one of various locations.
                        (let ((position-index
                               (-some (lambda (sym) (apigee--xml-locate-first-child-node proxy-endpoint-elt sym))
                                        '(PostClientFlow Flows PreFlow DefaultFaultRule FaultRules)))
                              (new-postflow-elt
                               (car (apigee--parse-xml "<PostFlow><Request/><Response/></PostFlow>"))))
                          ;; insert before the selected element
                          (push new-postflow-elt (cdr (nthcdr position-index proxy-endpoint-elt)))
                          (setq postflow-elt (car (xml-get-children proxy-endpoint-elt 'PostFlow)))))

                    (if preflow-elt
                        (let* ((preflow-response-elt (car (xml-get-children preflow-elt 'Response))))

                          (if (not preflow-response-elt)
                              (let ((new-preflow-response-elt (apigee--parse-xml "<Response></Response>")))
                                ;; insert as last element in PreFlow
                                (setcdr (last preflow-elt) new-preflow-response-elt)
                                (setq preflow-response-elt (car (xml-get-children preflow-elt 'Response)))))

                          ;; append the appropriate step-reference as child to Response
                          (setcdr (last preflow-response-elt)
                                  (cons "\n    " (list (cadr step-references))))))


                    (if postflow-elt
                        (let ((postflow-response-elt (car (xml-get-children postflow-elt 'Response))))

                          (if (not postflow-response-elt)
                              (let ((new-postflow-response-elt (apigee--parse-xml "<Response></Response>")))
                                ;; insert as last element in PostFlow
                                (setcdr (last postflow-elt) new-postflow-response-elt)
                                (setq postflow-response-elt (car (xml-get-children postflow-elt 'Response)))))

                          ;; append the appropriate step-reference as child to Response
                          (setcdr (last postflow-response-elt)
                                  (cons "\n    " (list (car step-references))))))


                    (if (not default-faultrule-elt)
                        ;; insert DefaultFaultRule node. As above, try one of various locations.
                        (let ((position-index
                               (-some (lambda (sym) (apigee--xml-locate-first-child-node proxy-endpoint-elt sym))
                                               '(PreFlow PostFlow PostClientFlow Flows FaultRules)))
                              (new-dfr-elt
                               (car (apigee--parse-xml "<DefaultFaultRule><AlwaysEnforce>true</AlwaysEnforce></DefaultFaultRule>"))))
                          ;; insert before the selected element
                          (push new-dfr-elt (cdr (nthcdr position-index proxy-endpoint-elt)))
                          (setq default-faultrule-elt (car (xml-get-children proxy-endpoint-elt 'DefaultFaultRule)))))

                    (if default-faultrule-elt
                        ;; append the step-references as children to Response
                        (setcdr (last default-faultrule-elt)
                                (cons "\n    " (list (car step-references)))))

                    (apigee--serialize-xml-elt root))
                  (save-buffer 0)))))))))


(defun apigee--xml-locate-first-child-node (node child-name)
  (let ((n -1)
        (count))
      (dolist (child (xml-node-children node))
        (setq n (+ 1 n))
        (if (and (listp child)
                 (equal (xml-node-name child) child-name))
          (setq count n)))
      count))

;; (defun apigee--diag-parse ()
;;   (interactive)
;;   (let*
;;       ((root (xml-parse-region (point-min) (point-max)))
;;        (proxy-endpoint-elt (car root))
;;        (first (-first (lambda (sym) (apigee--xml-locate-first-child-node proxy-endpoint-elt sym))
;;                       '(PreFlow PostFlow PostClientFlow Flows FaultRules)))
;;        (position-index (apigee--xml-locate-first-child-node proxy-endpoint-elt first))
;;        (the-cdr (cdr (nthcdr position-index proxy-endpoint-elt))))))
;;
;;
;;     ;;    (faultrules-elt (car (xml-get-children proxy-endpoint-elt 'FaultRules)))
;;     ;;    (default-faultrule-elt (car (xml-get-children proxy-endpoint-elt 'DefaultFaultRule)))
;;     ;;    (httpproxyconnection-elt (car (xml-get-children proxy-endpoint-elt 'HTTPProxyConnection)))
;;     ;;    (preflow-elt (car (xml-get-children proxy-endpoint-elt 'PreFlow)))
;;     ;;    (preflow-response-elt (car (xml-get-children preflow-elt 'Response)))
;;     ;;    (postclientflow-elt (car (xml-get-children proxy-endpoint-elt 'PostClientFlow)))
;;     ;;    (postflow-elt (car (xml-get-children proxy-endpoint-elt 'PostFlow)))
;;     ;;    (flows-elt (car (xml-get-children proxy-endpoint-elt 'Flows)))
;;     ;;    (dest-elt (or default-faultrule-elt faultrules-elt httpproxyconnection-elt))
;;     ;;    (new-preflow-elt
;;     ;;     (car (apigee--parse-xml "<PreFlow><Request/><Response/></PreFlow>"))))
;;     ;; ;;(setcdr new-preflow-elt (cddr dest-elt))
;;     ;; (setcdr (cddr dest-elt) new-preflow-elt)
;;     ;; (apigee--serialize-xml-elt root)))



(defun apigee--parse-xml (xml-string)
  (with-temp-buffer
    (insert xml-string)
    (xml--parse-buffer nil nil)))


(defun apigee--elaborate-template (fullpath-template-filename thing-name)
  "returns the contents of the named template with the ## placeholders
replaced."
  (let ((raw-template (apigee--get-template-contents fullpath-template-filename)))
    ;;(progn)
    (while (string-match "##" raw-template)
      (setq raw-template (replace-match thing-name t t raw-template)))
    raw-template))


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
    (when choice ;; is a cons cell
      (let ((policy-dir (concat bundle-dir bundle-type "/policies/"))
            (ptype (car choice))
            (template-filename (cdr choice))
            (have-name nil)
            (policy-name-prompt "policy name: "))
        (and (not (file-exists-p policy-dir))
             (make-directory policy-dir))
        (let* ((default-value (apigee--suggested-policy-name ptype template-filename))
               (policy-name
                (let (n)
                  (while (not have-name)
                    (setq n (read-string policy-name-prompt default-value nil default-value)
                          have-name (apigee--policy-name-is-available n)
                          policy-name-prompt "That name is in use. Policy name: " ))
                  n))
               (fullpath-template-filename
                (apigee--join-path-elements apigee--base-template-dir "policies" ptype template-filename))
               (elaborated-contents
                (apigee--elaborate-template fullpath-template-filename policy-name)))

          ;; create the file, expand the snippet, save it.
          (find-file (concat policy-dir policy-name ".xml"))
          ;; yas-expand-snippet-sync does not return until the snip is expanded.
          (yas-expand-snippet-sync elaborated-contents (point) (point))
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
          )))))

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

(defun apigee--verify-exactly-one (file-list source-dir)
  "verifies that there is exactly one xml file."
      (if (not file-list)
          (error
           (message "[apigee] cannot find XML file in %s, in `apigee--verify-exactly-one'" source-dir)))
      (if (not (= (length file-list) 1))
          (error
           (message "[apigee] found more than one XML file in %s, in `apigee--verify-exactly-one'" source-dir)))
      (nth 0 file-list))


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

    (let ((file-list (apigee--proper-files source-apiproxy-dir ".xml")))
      (apigee--verify-exactly-one file-list source-apiproxy-dir)
      ;; create the toplevel destination directory
      (make-directory dest-apiproxy-dir t)
      ;; copy, then intelligently modify the toplevel proxy bundle definition file
      (let ((new-xml-file-name (apigee--join-path-elements dest-apiproxy-dir (concat proxy-name ".xml"))))
        (copy-file (car file-list) new-xml-file-name nil t t nil)
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
           (file-list (apigee--proper-files dest-proxyendpoints-dir ".xml")))
      (if (not file-list)
          (error
           (message "[apigee] cannot find XML file in %s, in `apigee--copy-proxy-template-files'" dest-proxyendpoints-dir)))
      (if (= (length file-list) 1)
          (with-current-buffer (find-file-noselect (car file-list))
            (let*
                ((root (xml-parse-region (point-min) (point-max)))
                 (proxy-endpoint-elt (car root))
                 (http-proxy-conn-elt (car (xml-get-children proxy-endpoint-elt 'HTTPProxyConnection)))
                 (basepath-elt (car (xml-get-children http-proxy-conn-elt 'BasePath))))
              (setcar (cddr basepath-elt) (concat "/" proxy-name))
              (apigee--serialize-xml-elt root)
              )
            (save-buffer 0))))
    ))

(defun apigee--copy-sharedflow-template-files (sf-name source destination)
  "copy files from the SOURCE template directory to the DESTINATION directory,
changing names and replacing / expanding things as appropriate."
  (let ((source-sf-dir (apigee--join-path-elements source "sharedflowbundle"))
        (dest-sf-dir (apigee--join-path-elements destination "sharedflowbundle")))
    (let ((file-list (apigee--proper-files source-sf-dir ".xml")))
      (apigee--verify-exactly-one file-list source-sf-dir)
      (make-directory dest-sf-dir t)
      ;; copy, then intelligently modify the toplevel proxy bundle definition file
      (let ((new-xml-file-name (apigee--join-path-elements dest-sf-dir (concat sf-name ".xml"))))
        (copy-file (car file-list) new-xml-file-name nil t t nil)
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

     ;; override the impl in popup.el - it's broken for me for cascaded menus - 20230627-1541
     (defun popup-child-point (popup &optional offset)
       (overlay-end
        (popup-line-overlay popup (popup-selected-line popup))))

     ;; customize the popup font
     (set-face-attribute 'popup-menu-face nil :height 105)
     (set-face-attribute 'popup-menu-selection-face nil :height 105)

     (apigee--restore-state)
     (if (not apigee--base-template-dir)
         (setq apigee--base-template-dir (concat (file-name-directory apigee--load-file-name) "templates/")))
     (if apigee--base-template-dir
         (apigee-load-templates apigee--base-template-dir))
     (setq apigee-timer
           (run-with-timer (* 60 apigee--timer-minutes) (* 60 apigee--timer-minutes) 'apigee--persist-state))))

(provide 'apigee)

;;; apigee.el ends here
