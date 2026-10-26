;;; -*- coding: utf-8; lexical-binding: t;  -*-

;;; apigee.el --- utility functions for working with Apigee platform in emacs
;;
;; Copyright (C) 2017-2025 Dino Chiesa and Google, LLC.
;;
;; Author     : Dino Chiesa
;; Maintainer : Dino Chiesa <dchiesa@google.com>
;; Created    : May 2017
;; Modified   : May 2025
;; Version    : 1.3
;; Keywords   : apigee
;; Requires   : s.el, xml.el
;; License    : Apache 2.0
;; X-URL      : https://github.com/DinoChiesa/apigee-el
;; Last-saved : <2026-February-22 18:28:01>
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
;; (use-package apigee
;;   :if (file-exists-p "~/elisp/apigee/apigee.el")
;;   :load-path "~/elisp/apigee"
;;   :defer t
;;   :commands (apigee-new-proxy apigee-lint-asset)
;;   :config
;;     ...)
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
;;    Copyright © 2017-2026 Google LLC.
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
(require 'xml)
(require 'compile)
(require 'dc-xml)
(require 'dc-sane-sorting)
(require 'xml-to-string)
(require 'dash) ;; magnars' functional lib, functions start with dash
(require 'cl-lib) ; For assoc-if

;; Declared special so that `let' binds them dynamically even when this file
;; is byte-compiled, matching how they behave when it is loaded from source.
(defvar tramp-connection-properties)
(defvar tramp-default-remote-shell)
(defvar tramp-encoding-shell)
(defvar comint-last-output-start)

(defvar apigee-function-cache (make-hash-table :test 'equal)
  "Cache for memoizing functions with timeouts.")

(defvar apigee--base-template-dir nil
  "The directory from which policy templates have been most recently loaded")

(defvar apigee--recently-used-asset-homes nil
  "a list of directories recently used to store API Proxies.")

(defvar apigee--verbose-logging nil
  "whether this module should log verbosely into *Messages*. This is an on/off
variable. Set it to a truthy value to get logging.")

(defvar apigee--timer-minutes 6
  "length of the interval in minutes between persisting apigee-el settings.")

(defvar apigee-timer nil
  "cancellable timer, for saving apigee-emacs settings.")

(defvar apigee--list-of-vars-to-store-and-restore
  (list "apigee--verbose-logging" "apigee--recently-used-asset-homes"
        "apigee--base-template-dir" "apigee--timer-minutes" "apigee-organization"
        "apigee-environment" "apigee-service-account")
  "a list of variables to store/restore in the settings file.")

(defvar apigee--settings-file-base-name "apigee-el.dat")

(defvar apigee-xmlschema-validator-home "Apigee-Policy-Schema"
  "The directory that holds the Apigee policy schema and validator tool.
If this is a relative directory, it will be resolved relative to the
directory containing the apigee.el file.
You need to have created the venv (in either .venv or venv) and installed
the prerequisites via `pip install -r requirements.txt`")

(defvar apigee-commands-alist
  '(
    (import . "%apigeecli apis create bundle -f apiproxy --name %n -o %o --token %t")
    (deploy . "%apigeecli apis deploy --wait --name %n --ovr --org %o --env %e %sa_args --token %t")
    (import-and-deploy . "%apigeecli apis create bundle -f apiproxy --name %n -o %o --token %t %c %apigeecli apis deploy --wait --name %n --ovr --org %o --env %e %sa_args --token %t")
    (lint .  "%apigeelint -s . -e %lint-exclusions -f visualstudio.js")
    (validate .  "%validate-cmd --source .")
    ))

;; To modify one of the above after program starts:
;; (setf (alist-get 'lint apigee-commands-alist)
;;      "%apigeelint -s . -e %lint-exclusions -f visualstudio.js")

(defvar apigee-programs-alist
  '(
    (gcloud . "gcloud")
    (apigeecli . "apigeecli")
    (apigeelint . ("node" "~/path/to/apigeelint/cli.js"))
    (validate-cmd . apigee--get-xmlschema-validator-program)
    )
  "apigee.el will use `executable-find' to locate the programs on the path. If
you want to avoid that, you can set the path explicitly in your emacs init file,
via, for example,
  (setf (alist-get 'apigeecli apigee-programs-alist) \"~/.apigeecli/bin/apigeecli\"
        (alist-get 'gcloud apigee-programs-alist) \"~/gcloud-sdk/bin/gcloud\"
    )"
  )

(defvar apigee--memoized-gcloud-pat
  nil
  "memoized version of print-access-token")

(defvar apigee-placeholders-alist
  `(
    (c               . apigee--concatenate-command-string)
    (n               . apigee--bundle-name)
    (o               . apigee-get-organization)
    (e               . apigee-get-environment )
    (lint-exclusions . apigee-get-lint-exclusions )
    (sa_args         . apigee-get-service-account-args-for-deployment )
    (t               . apigee--memoized-gcloud-pat)
    )
  "functions that return values to be used as placeholders in commands"
  )

;; useful during development
;;(add-to-list 'apigee-placeholders-alist '(lint-exclusions . apigee-get-lint-exclusions))


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

(defvar apigee-organization nil
  "The organization to use for deployments. apigee.el will prompt if unset.")
(defvar apigee-environment nil
  "The environment to use for deployments. apigee.el will prompt if unset.")
(defvar apigee-service-account nil
  "The Service Account to use for deployments. The format must be {ACCOUNT_ID}@{PROJECT}.iam.gserviceaccount.com.")

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


(defvar apigee-lint-exclusions "TD002,TD007,TD012"
  "The comma-separated list of plugins to exclude from apigeelint")

(defvar apigee--policy-template-alist nil
  "An alist. For each element, the cons is a policy type name, eg
\"AccessEntity\", and the cadr is a list of strings, each of which is a
filename referring to a template for the policy. The actual filename
will be in (apigee--join-path-elements apigee--base-template-dir
\"policies\" policy-type filename)")

(defvar apigee--proxy-template-alist nil
  "An alist, relating a name to a proxy template. The cadr of each entry is a
string, a directory name, containing the exploded proxy template.")

(defvar apigee--target-template-alist nil
  "An alist, relating a name to a target template. The cadr of each entry is a
string, a file name, containing the target template.")

(defvar apigee--sharedflow-template-alist nil
  "An alist, relating a name to a template for a sharedflow. The cadr of each
entry is a string, a directory name, containing the exploded sharedflow
template.")


(defun apigee-get-lint-exclusions (&optional want-prompt)
  "get a string representing a comma-separated set of plugins to
exclude from apigeelint. Uses a cached value, or, if WANT-PROMPT, will
prompt the user."
  (interactive "P")
  (let ((excl-local
         (if want-prompt
             (apigee--read-or-default "exclusions"
                                      apigee-lint-exclusions)
           apigee-lint-exclusions)))
    (setq apigee-lint-exclusions excl-local)))


(defun apigee--path-to-settings-file ()
  "a function rturning the path to the settings file for apigee.el"
  (apigee--join-path-elements user-emacs-directory apigee--settings-file-base-name))

(defun apigee--restore-state ()
  "function expected to be called on initial module load, that restores the
previous state of the module. Things like the most recently used
apiproxy home, or the most recently loaded templates directory."
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
  "This function stores the state of the module. Things like the most
recently used apiproxy home, the most recently used org and environment,
or the most recently loaded templates directory. See the list of
settings in `apigee--list-of-vars-to-store-and-restore'.

It is expected that this function will be called periodically."
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
        (pp alist-to-write (current-buffer))))))

;; (defun apigee--join-path-elements (root &rest dirs)
;;   "Joins a series of directories together, inserting slashes as necessary,
;; like Python's os.path.join. TODO: figure out why I should not just use
;; `file-name-concat'"
;;   (if (not dirs)
;;       root
;;     (apply 'apigee--join-path-elements
;;            (let ((first (car dirs)))
;;              (if (s-suffix? "/" root)
;;                  (concat root
;;                          (if (s-prefix? "/" first)
;;                              (substring first 1 (length first))
;;                            first))
;;                (if (s-prefix? "/" first)
;;                    (concat root first)
;;                  (concat root "/" (car dirs)))))
;;            (cdr dirs))))

(defun apigee--join-path-elements (root &rest dirs)
  "Joins a series of directories together, starting at the root, inserting
slashes as necessary, as with Python's os.path.join."
  (apply #'file-name-concat root dirs))

(defun apigee--insure-trailing-slash (path)
  "Insure the given path ends with a slash. This is usedful with
`default-directory'. Setting `default-directory' to a value that
does not end with a slash causes it to use the parent directory."
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
  "loads the policy templates for one POLICY-TYPE-DIRECTORY, which is a fully-
qualified directory, ending in a policy-type name eg AccessEntity.  This
is used to populate the menu of available policy templates."
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
  "Load templates for a kind of asset from the template dir,
then set the symbol to the alist."
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

The `apigee--policy-template-alist' gets set with something like this:

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
to fill in for a new policy."
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

Each child under the template directory should be a .xml file defining a
target. The name of the file will be the name of the template in the
resulting menu."

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
Within each of those, there will be templates for those entities."
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

(defun apigee--read-or-default (prompt-label default-val)
  "Prompt with PROMPT-LABEL, pre-filling DEFAULT-VAL if it is not blank.
Return the input, or DEFAULT-VAL if the input is empty. Return nil only
when both are empty, never the empty string."
  (let ((response-val
         (if (s-blank? default-val)
             (read-string (format "%s: " prompt-label))
           (read-string (format "%s (%s): " prompt-label default-val)
                        default-val))))
    (if (string-empty-p response-val)
        (and (not (s-blank? default-val)) default-val)
      response-val)))

(defun apigee-get-organization (&optional want-prompt)
  "Returns the org to use for import and deployment. Uses a cached
value, or, if WANT-PROMPT, will prompt the user."
  (interactive "P")
  (let ((org-local
         (if (or want-prompt (s-blank? apigee-organization))
             (apigee--read-or-default "organization name"
                                      apigee-organization)
           apigee-organization)))
    (when (s-blank? org-local)
      (user-error "[apigee] An organization name is required"))
    (setq apigee-organization org-local)))

(defun apigee-get-environment (&optional want-prompt)
  "Returns the environment to use for deployment. Uses a cached value,
or, if WANT-PROMPT, will prompt the user."
  (interactive "P")
  (let ((env-local
         (if (or want-prompt (s-blank? apigee-environment))
             (apigee--read-or-default "environment name"
                                      apigee-environment)
           apigee-environment)))
    (when (s-blank? env-local)
      (user-error "[apigee] An environment name is required"))
    (setq apigee-environment env-local)))

(defun apigee-get-service-account-args-for-deployment (&optional want-prompt)
  "Returns the arguments to use on the apigeecli command line for the
service account. Uses a cached value, or, if WANT-PROMPT, will
prompt the user. The format of the service account must be
{ACCOUNT_ID}@{PROJECT}.iam.gserviceaccount.com . If the format is just
the short version, this fn will append the PROJECT and suffix, using the
Apigee organization name as the PROJECT. That holds for Apigee X and
hybrid, where the organization is named after its GCP project.

Enter \"-\" to deploy without a service account. That choice is stored
like any other value, so it is not asked again.

Returns \"--sa EMAIL\", or the empty string for no service account."
  (interactive "P")
  (let ((sa-local
         (if (or want-prompt (not apigee-service-account))
             (apigee--read-or-default "service account"
                                      apigee-service-account)
           apigee-service-account))
        (no-sa-p (lambda (sa) (or (s-blank? sa) (equal sa "-")))))
    (when (and (not (funcall no-sa-p sa-local)) (not (s-contains? "@" sa-local)))
      ;; user provided short form; append the boilerplate suffix.
      (setq sa-local (format "%s@%s.iam.gserviceaccount.com" sa-local (apigee-get-organization) )))
    (setq apigee-service-account sa-local) ;; for next time, and for persistence
    (if (funcall no-sa-p sa-local)
        ""
      (format "--sa %s" sa-local))))

(defun apigee--trim-xml-suffix (s)
  "trims the .xml suffix from a template file name"
  (s-chop-suffix ".xml" s))

;; 20250527-1816 - not needed now that I'm using the minibuffer
;; (defun apigee--get-menu-position ()
;;   "get the position for the popup menu"
;;   (if (fboundp 'posn-at-point)
;;       (let ((x-y (posn-x-y (posn-at-point (point)))))
;;         (list (list (+ (car x-y) 10)
;;                     (+ (cdr x-y) 20))
;;               (selected-window)))
;;     t))

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

If not in a bundle, then it returns....nil
"
  (let ((path
         (apigee--insure-trailing-slash
          (car (cl-some 'apigee--find-bundle '("apiproxy" "sharedflowbundle"))))))
    (and path (file-truename path))))

(defun apigee--require-bundle ()
  "Return the root of the current bundle, or signal a `user-error'."
  (or (apigee--root-path-of-bundle)
      (user-error "[apigee] You do not appear to be in an API proxy or sharedflow directory")))

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
choose a target type to insert."
  (interactive)
  (apigee--require-apiproxy "Adding a target")
  (let ((apiproxy-dir (apigee--require-bundle))
        (template-name
         (completing-read
          "target template: "
          (dc-ss/completion-fn
           (mapcar (lambda (x) (car x)) apigee--target-template-alist)
           'sorted-sanely)
          nil t nil)))
    (progn
      (when (string-empty-p template-name)
        (user-error "[apigee] No target template chosen"))
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

(defun apigee--transform-policy-alist (input-alist)
  "Transforms the given alist according to the specified format.
Input: ((KEY (SUBKEY1.xml SUBKEY2.xml ...)) ...)
Output: ((KEY . ((SUBKEY1) (SUBKEY2) ...)) ...)

Suitable for use with `apigee--select-cascaded-choices'. "
  (mapcar
   (lambda (top-level-entry)
     (let ((key (car top-level-entry))
           (filename-list (cadr top-level-entry)))
       (cons key
             (mapcar
              (lambda (filename-string)
                (list (file-name-sans-extension filename-string)))
              filename-list))))
   input-alist))

(defun apigee--select-cascaded-choices (choices-data)
  "Presents a cascaded list of choices to the user using `completing-read'.
CHOICES-DATA is an alist where each element is (STRING . SUBLIST).
SUBLIST is another such alist, or nil/empty-list if STRING is a final choice.
Returns the path of selected items as a list of strings, or nil if aborted."
  (let ((current-level-data choices-data)
        (prompt-base "Select")
        (selected-path nil) ; Stores the actual selections (strings)
        ;; Stores the data for each level of the hierarchy we've entered
        (history-of-data-levels (list choices-data))
        user-final-selection)

    ;; Loop while there are valid choices at the current level
    (while (and current-level-data
                (listp current-level-data)
                (consp current-level-data)) ; Ensure current-level-data is a non-empty list
      (let* ((current-options (mapcar #'car current-level-data))
             (effective-options current-options)
             (is-top-level (eq current-level-data choices-data)))

        ;; Add "<Back>" option if not at the top level
        (unless is-top-level
          (setq effective-options (cons "<Back>" effective-options)))

        (let* ((prompt (if selected-path
                           (format "%s (%s): " prompt-base (mapconcat #'identity selected-path " > "))
                         (format "%s: " prompt-base)))
               (raw-selection (completing-read prompt
                                               (dc-ss/completion-fn effective-options 'sorted-sanely)
                                               nil t)))

          (cond
           ;; Empty input (RET with nothing typed): ask again at this level.
           ((string-empty-p raw-selection) nil)

           ((equal raw-selection "<Back>")
            (pop selected-path)
            (pop history-of-data-levels) ; Remove current level's data from history
            (setq current-level-data (car history-of-data-levels))) ; Set to previous data

           (t
            (let* ((selected-assoc (assoc raw-selection current-level-data))
                   (next-level-data (cdr selected-assoc)))

              (push raw-selection selected-path) ; Add current selection to path

              ;; Check if there are further sub-choices
              (if (and next-level-data (listp next-level-data) (consp next-level-data))
                  ;; There are more choices (next-level-data is a non-empty list of choices)
                  (progn
                    (setq current-level-data next-level-data)
                    (push current-level-data history-of-data-levels))
                ;; This is a final choice (next-level-data is nil, empty, or not a valid submenu)
                (progn
                  (setq user-final-selection (nreverse selected-path))
                  (setq current-level-data nil))))))))) ; Terminate loop by setting current-level-data to nil
    user-final-selection))

(defun apigee--prompt-user-with-policy-choices ()
  "Prompt the user with the available choices. In this context, the
available choices is the hierarchical list of available policies."
  (interactive)
  (apigee--select-cascaded-choices
   (apigee--transform-policy-alist apigee--policy-template-alist)))


(defun apigee--is-existing-directory (dir-name)
  "Tests to see whether a name refers to an existing directory.
A symbolic link to a directory counts."
  (file-directory-p dir-name))

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

(defun apigee-gcloud-auth-print-access-token (&rest ignored)
  "return output of $(gcloud auth print-access-token) .
It accepts an arg because the logic in `apigee--replace-placeholder'
expects it."
  (let* ((gcloud-pgm
          (alist-get 'gcloud apigee-programs-alist))
         (resolved-pgm (apigee--resolve-program gcloud-pgm)))
    (if (not resolved-pgm)
        (error (format "cannot resolve %s" gcloud-pgm)))
    (let* ((command-string (concat resolved-pgm " auth print-access-token"))
           (output (replace-regexp-in-string "\n$" "" (shell-command-to-string command-string)))
           (lines (split-string output "\n")))
      (car (last lines)))))

(defun apigee--memoize-with-timeout (fn &optional timeout-in-seconds)
  "Memoize a function with a timeout.

  FUNCTION is the function to memoize.
  TIMEOUT-IN-SECONDS is the cache expiration time in seconds (default:
600 seconds - 10 minutes).

  Returns a memoized function."
  (let ((timeout (or timeout-in-seconds (* 10 60)))  ; Default to 10 minutes
        (lexically-bound-fn fn))
    (lambda (&rest args)
      (let* ((key (cons lexically-bound-fn args))
             (entry (gethash key apigee-function-cache))
             (now (current-time)))
        (if (and entry
                 (<= (float-time (time-subtract now (cdr entry))) timeout))
            (car entry) ; Return cached value
          (let ((result (apply lexically-bound-fn args)))
            (puthash key (cons result now) apigee-function-cache) ; Update cache
            result))))))

(setq apigee--memoized-gcloud-pat (apigee--memoize-with-timeout #'apigee-gcloud-auth-print-access-token (* 1 60)))

(defun apigee--concatenate-command-string (&rest ignored)
  "Return the string that concatenates commands. On Linux this is
semicolon (;), on Windows, double ampersand (&&)."
  (cond
   ((eq system-type 'windows-nt) "&&")
   (t ";")))


(defun apigee--get-xmlschema-validator-program ()
  "Resolves the command for the XML Schema validator program.
It constructs the path to the Python executable within the virtual
environment and the ='validateBundle.py'= script, relative to
='apigee-xmlschema-validator-home'=."
  (if-let* ((apigee-el-dir (file-name-directory apigee--load-file-name))
            (base-dir (if (file-name-absolute-p apigee-xmlschema-validator-home)
                          apigee-xmlschema-validator-home
                        (file-truename (apigee--join-path-elements apigee-el-dir apigee-xmlschema-validator-home))))
            (_ (apigee--is-existing-directory base-dir))
            (py-dir (apigee--join-path-elements base-dir "py"))
            (_ (apigee--is-existing-directory py-dir))
            (venv-name (seq-find
                        (lambda (v)
                          (apigee--is-existing-directory (apigee--join-path-elements py-dir v)))
                        '(".venv" "venv")))
            (full-venv-dir (apigee--join-path-elements py-dir venv-name)) ; This is the path to the .venv or venv directory itself
            (validate-script (apigee--join-path-elements py-dir "validateBundle.py"))
            (_ (file-exists-p validate-script))) ; Check if validateBundle.py exists
      (let* ((python-executable-suffix
              (cond
               ((memq system-type '(gnu/linux darwin)) "bin/python") ; Linux, macOS, WSL
               ((eq system-type 'windows-nt) "Scripts/python.exe") ; Native Windows Emacs
               (t "python"))) ; Fallback or other systems
             (python-path (apigee--join-path-elements full-venv-dir python-executable-suffix)))
        (if (file-exists-p python-path)
            (format "%s %s" python-path validate-script)
          (error (format "Python executable not found at %s" python-path))))
    (error "Could not resolve XML Schema validator program. Check 'apigee-xmlschema-validator-home'")))

(defun apigee--bundle-name (&rest _)
  "Return the short name of the current API proxy or sharedflow.
The name comes from the single top-level XML file in the bundle
directory. It accepts arguments because `apigee--replace-placeholder'
passes one."
  (when-let* ((bundle-root (apigee--root-path-of-bundle))
              (bundle-type (apigee--type-of-bundle)))
    (let* ((bundle-dir (concat bundle-root bundle-type))
           (file-list (apigee--proper-files bundle-dir ".xml"))
           (defn-file (apigee--verify-exactly-one file-list bundle-dir)))
      (apigee--trim-xml-suffix (file-name-nondirectory defn-file)))))

(defun apigee--require-apiproxy (action)
  "Signal a `user-error' if the current bundle is a sharedflow.
ACTION names what the user tried, for the message. The import and deploy
commands in `apigee-commands-alist' use \"apigeecli apis\" and so work
only for API proxies."
  (when (equal (apigee--type-of-bundle) "sharedflowbundle")
    (user-error "[apigee] %s supports only API proxies, not sharedflows" action)))

(defun apigee--substitute-placeholder (key value template)
  "Replace each %KEY in TEMPLATE with VALUE, taken literally.
%KEY must not be followed by a name character, so %t does not match the
start of %token."
  (replace-regexp-in-string
   (concat "\\(%" (regexp-quote (symbol-name key)) "\\)\\(?:[^[:alnum:]_-]\\|\\'\\)")
   value template t t 1))

(defun apigee--replace-placeholder (want-prompt)
  "used in seq-reduce. I defined this in a formal fn rather
than a lambda to avoid edebug issues."
  (let ((want-prompt want-prompt))
    (lambda (acc item)
      (let* ((key (car item))
             (replaceable (concat "%" (symbol-name key))))
        (if (s-contains? replaceable acc)
            (if-let*
                ((replacer-fn (cdr item))
                 (callable-fn ;; the cdr is either a defun or a variable
                  (cond ((fboundp replacer-fn) replacer-fn)
                        ((and (boundp replacer-fn) (functionp (symbol-value replacer-fn)))
                         (symbol-value replacer-fn))
                        (t nil)))
                 (replacement (funcall callable-fn want-prompt)))
                (apigee--substitute-placeholder key replacement acc)
              acc)
          acc)))))

(defun apigee--find-executable (name)
  "Return the executable NAME as a shell-quoted absolute path, or nil.
A NAME with a directory part is checked directly; otherwise it is looked
up with `executable-find'."
  (when-let* ((path (if (file-name-directory name)
                        (let ((expanded (expand-file-name name)))
                          (and (file-executable-p expanded)
                               (not (file-directory-p expanded))
                               expanded))
                      (executable-find name))))
    (shell-quote-argument path)))

(defun apigee--resolve-program (pgm-arg)
  "Resolve the program. PGM-ARG is either a function, a list, or a string.
If a function, then invoke it and take the string value of
that. If a list, resolve the first arg as an executable and then concatenate
that with the remaining args. If a string, resolve it as an executable; a
path with spaces works. Failing that, treat the first word as the
executable and the rest as arguments. Return nil if the executable can't
be found."
  (cond
   ((functionp pgm-arg)
    (funcall pgm-arg))
   ((consp pgm-arg)
    (when-let* ((executable (apigee--find-executable (car pgm-arg))))
      (string-join (cons executable (cdr pgm-arg)) " ")))
   ((stringp pgm-arg)
    (or (apigee--find-executable pgm-arg)
        (let ((words (split-string pgm-arg " " t)))
          (when-let* ((executable (and words (apigee--find-executable (car words)))))
            (string-join (cons executable (cdr words)) " ")))))))



(defun apigee--replace-program (acc item)
  "Helper for seq-reduce. Replaces placeholders in ACC with resolved program paths.
ITEM is a cons cell from `apigee-programs-alist` (KEY . VALUE).  VALUE
can be a string (the program name/path) or a function symbol that, when
called, returns the program name/path string."
  (let* ((key (car item))
         (program-val (cdr item)) ; Can be a string or a function symbol
         (replaceable (concat "%" (symbol-name key))))
    (if (s-contains? replaceable acc)
        (progn
          (let ((resolved-pgm (apigee--resolve-program program-val)))
            (if (not resolved-pgm)
                (error (format "cannot resolve program '%s'" program-val)))
            (apigee--substitute-placeholder key resolved-pgm acc)))
      acc)))

(defun apigee--get-command (symbol want-prompt)
  "get the command to run (eg apigeelint, apigeecli, etc) for the
current API proxy."
  (let ((cmd (alist-get symbol apigee-commands-alist)))
    (when cmd
      (setq cmd
            (seq-reduce #'apigee--replace-program apigee-programs-alist cmd))
      ;; I had trouble using this seq-reduce, when using edebug-defun.
      ;; But in my experience, when NOT using the debugger, it works as intended.
      ;; Womp womp.
      (let ((replacer (apigee--replace-placeholder want-prompt)))
        (seq-reduce replacer apigee-placeholders-alist cmd)))))

(defconst apigee--token-arg-regexp "--token \\([^ \n]+\\)"
  "Matches a --token argument and its value.")

(defconst apigee--masked-token-arg "--token ***masked***"
  "Replacement text for a --token argument.")

(defun apigee--mask-tokens-from (start)
  "Mask --token arguments in the current buffer from START to the end."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char start)
      (while (re-search-forward apigee--token-arg-regexp nil t)
        (replace-match apigee--masked-token-arg t t)))))

(defun apigee--mask-secrets (&rest _)
  "Mask tokens in the newest process output.
For buffer-local use in `comint-output-filter-functions'. The apigee
commands run `compilation-start' in comint mode, where
`compilation-filter-hook' never runs. Rescanning from the start of the
line catches a token split across two output chunks."
  (apigee--mask-tokens-from
   (save-excursion (goto-char comint-last-output-start) (line-beginning-position))))

(defun apigee--mask-command-echo (&rest _)
  "Mask tokens in the whole buffer, including the echoed command line.
For buffer-local use in `compilation-start-hook', so that a recompile
is masked too."
  (apigee--mask-tokens-from (point-min)))

;; Survive `kill-all-local-variables', e.g. when the user recompiles with g.
(put 'apigee--mask-secrets 'permanent-local-hook t)
(put 'apigee--mask-command-echo 'permanent-local-hook t)

(defun apigee--mask-token (unmasked-cmd)
  "Masks tokens in a command. See also
 `apigee--reinsert-token'"
  (replace-regexp-in-string apigee--token-arg-regexp apigee--masked-token-arg unmasked-cmd t t))

(defun apigee--reinsert-token (masked-cmd)
  "Re-inserts tokens into a masked command. See also
 `apigee--mask-token'"
  (let ((callable-fn
         (symbol-value 'apigee--memoized-gcloud-pat)))
    (replace-regexp-in-string (regexp-quote apigee--masked-token-arg)
                              (concat "--token " (funcall callable-fn nil))
                              masked-cmd t t)))


(defun apigee--has-same-bundle-root (root-dir-in-question)
  "predicate called in each unsaved buffer.  Returns t if the buffer
is part of an apiproxy, and if the ROOT-DIR-IN-QUESTION is the same
as the root of the current buffer.
Used as a predicate for `save-some-buffers'."
  (let ((bundle-dir (apigee--root-path-of-bundle)))
    (and bundle-dir
         (not (s-blank? bundle-dir))
         (equal bundle-dir root-dir-in-question))))

(defun dc-find-item-in-list-of-lists (list-of-lists target-string)
  "Find the first item in LIST-OF-LISTS whose car matches TARGET-STRING.
Returns nil if no such item is found."
  (catch 'found
    (dolist (item list-of-lists)
      (when (string= (car item) target-string)
        (throw 'found item)))))

(defun apigee--run-command-for-proxy (label command-symbol &optional want-prompt)
  "Run a command for the current API proxy or sharedflow."
  (if-let* ((bundle-dir (apigee--root-path-of-bundle)))
      (if-let* ((bundle-name (apigee--bundle-name))
                (cmd (apigee--get-command command-symbol want-prompt)))
          (let ((name-function (lambda (mode) (concat "*" label " - " (symbol-name command-symbol) " - " bundle-name "*")))
                (remote-host
                 (save-match-data ; is usually a good idea
                   (and
                    (string-match "^/ssh:\\([^:]+\\):" bundle-dir)
                    (match-string 1 bundle-dir))))
                highlight-regexp)
            (when cmd
              (save-some-buffers nil (lambda () (apigee--has-same-bundle-root bundle-dir)))
              (let ((default-directory (expand-file-name bundle-dir))
                    ;;(process-environment '("PATH=foobar"))
                    )
                ;; Chapter 1. trying to get compilation-start over Tramp to use bash
                (when remote-host
                  (let* ((ssh-spec-pattern (concat "/ssh:" remote-host ":"))
                         (safe-ssh-spec-pattern (regexp-quote ssh-spec-pattern))
                         (item
                          (dc-find-item-in-list-of-lists tramp-connection-properties safe-ssh-spec-pattern)))
                    (if (not item)
                        (add-to-list 'tramp-connection-properties
                                     (list safe-ssh-spec-pattern
                                           "remote-shell" "/bin/bash")))))
                ;; 20250301-0426
                ;;
                ;; If I use nil here for mode (later: mode?), I cannot mask the token, and the output
                ;; is not displayed very quickly. If I use t, then the code can mask
                ;; the token, and the output gets displayed much more quickly. The
                ;; experience is better.  The one downside: the compilation is shown in
                ;; comint mode, which I theoretically don't want - it's not
                ;; interactive. But I don't care that much.
                (with-connection-local-variables
                 (let (
                       ;; Chapter 2. trying to get compilation-start over Tramp to use bash.
                       ;; Not sure which, if any, are required here.
                       (tramp-default-remote-shell "/bin/bash")
                       (tramp-encoding-shell "/bin/bash"))
                   (let* ((actual-command-to-run
                           ;; If a prefix was used, prompt the user to edit the command.
                           ;; But mask the token during editing.
                           (if want-prompt
                               (let* ((command-with-token-masked (apigee--mask-token cmd))
                                      (confirmed-command-to-run
                                       (read-shell-command "Run build command: " command-with-token-masked)))
                                 (apigee--reinsert-token confirmed-command-to-run))
                             cmd))
                          (buf
                           (compilation-start
                            ;; 20251115-1240
                            ;; it seems comint sets a default-directory without the need for cd.
                            ;;(concat "cd " bundle-dir "; " actual-command-to-run)
                            actual-command-to-run
                            t name-function highlight-regexp)))
                     (with-current-buffer buf
                       ;; obscure any token that appears, now and in later output
                       (apigee--mask-tokens-from (point-min))
                       (add-hook 'comint-output-filter-functions #'apigee--mask-secrets nil t)
                       (add-hook 'compilation-start-hook #'apigee--mask-command-echo nil t)
                       (setq buffer-read-only t))))))))
        (error "Can't find bundle name, or can't resolve command."))
    (error "You do not appear to be in an API proxy or sharedflow directory.")))


;; (compilation-start (concat "cd " bundle-dir "; " cmd) t name-function highlight-regexp)
;; ;;(run-with-timer 6 nil #'apigee--remove-compilation-filter-hook )
;; ))))

;; ;; if I use nil here for mode, I cannot mask the token
;; ;; if I yuse t, then the code can mask the token.
;; ;; is it a race condition?
;; (let ((buf
;;        (compilation-start (concat "cd " bundle-dir "; " cmd) nil name-function highlight-regexp)))
;;   (with-current-buffer buf
;;     ;; obscure any token that appears
;;     (save-excursion
;;       (beginning-of-buffer)
;;       (while (re-search-forward "--token \\([^ \n]+\\)" nil t)
;;         (replace-match "--token ***masked***")))
;;     (setq buffer-read-only t)))))))

(defun apigee-lint-asset (have-prefix)
  "Run apigeelint on the current API proxy or sharedflow.

With a prefix argument (e.g., C-u), prompt the user to edit
the command before running it."
  (interactive "P")
  (apigee--run-command-for-proxy "apigeelint" 'lint have-prefix))

(defun apigee-validate-xml-for-asset (have-prefix)
  "Run the XML Validator on the current API proxy or sharedflow.

With a prefix argument (e.g., C-u), prompt the user to edit
the command before running it."
  (interactive "P")
  (apigee--run-command-for-proxy "validate" 'validate have-prefix))

;;;###autoload
(defun apigee-import-and-deploy-proxy (have-prefix)
  "Import AND deploy the API proxy.

With a prefix argument (e.g., C-u), prompt the user for org/env
before running it."
  (interactive "P") ;; if have-prefix is non-nil, prompt the user for org/env
  (apigee--require-apiproxy "Import and deploy")
  (apigee--run-command-for-proxy "apigeecli" 'import-and-deploy have-prefix))

;;;###autoload
(defun apigee-import-proxy (have-prefix)
  "Import the API proxy."
  (interactive "P") ;; if have-prefix is non-nil, prompt the user for org/env
  (apigee--require-apiproxy "Import")
  (apigee--run-command-for-proxy "apigeecli" 'import have-prefix))

;;;###autoload
(defun apigee-deploy-proxy (have-prefix)
  "Deploy the API proxy. This doesn't use the proxy configuration in the
filesystem, beyond the name. It's assumed you've recently called
`apigee-import-proxy', and there is a revision to deploy."
  (interactive "P") ;; if have-prefix is non-nil, prompt the user for org/env
  (apigee--require-apiproxy "Deploy")
  (apigee--run-command-for-proxy "apigeecli" 'deploy have-prefix))

;;;###autoload
(defun apigee-inject-proxy-revision-logic ()
  "Inject the policies and flow to insert a proxy revision header
into the response. Handy when modifying an existing proxy that
does not send back a revision header, or when creating a proxy
from a template that doesn't send back a revision header.

Running it again on a proxy that already has the logic changes nothing."
  (interactive)
  (let ((bundle-dir (apigee--root-path-of-bundle))
        (bundle-type (apigee--type-of-bundle)))
    (if (s-equals? bundle-type "apiproxy")
        (let* ((bundle-policy-dir (concat bundle-dir bundle-type "/policies/"))
               (clean-headers-policy
                (apigee--ensure-assign-message-policy
                 "clean request headers from response.xml" bundle-policy-dir))
               (revision-header-policy
                (apigee--ensure-assign-message-policy
                 "inject proxy revision header.xml" bundle-policy-dir)))

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
                       (postflow-elt (car (xml-get-children proxy-endpoint-elt 'PostFlow))))

                    (unless preflow-elt
                      ;; insert PreFlow node, try one of various locations.
                      (setq preflow-elt (car (apigee--parse-xml "<PreFlow><Request/><Response/></PreFlow>")))
                      (apigee--xml-insert-child-before
                       proxy-endpoint-elt preflow-elt
                       '(PostFlow PostClientFlow Flows DefaultFaultRule FaultRules)))

                    (unless postflow-elt
                      ;; insert PostFlow node, try one of various locations.
                      (setq postflow-elt (car (apigee--parse-xml "<PostFlow><Request/><Response/></PostFlow>")))
                      (apigee--xml-insert-child-before
                       proxy-endpoint-elt postflow-elt
                       '(PostClientFlow Flows PreFlow DefaultFaultRule FaultRules)))

                    (unless default-faultrule-elt
                      ;; insert DefaultFaultRule node. As above, try one of various locations.
                      (setq default-faultrule-elt
                            (car (apigee--parse-xml "<DefaultFaultRule><AlwaysEnforce>true</AlwaysEnforce></DefaultFaultRule>")))
                      (apigee--xml-insert-child-before
                       proxy-endpoint-elt default-faultrule-elt
                       '(PreFlow PostFlow PostClientFlow Flows FaultRules)))

                    ;; Every append is evaluated; any one of them adding a Step
                    ;; means the file changed. A newly inserted flow element
                    ;; always gets a Step, so it counts too.
                    (let ((changes
                           (list
                            (apigee--xml-append-step
                             (apigee--xml-flow-response preflow-elt) clean-headers-policy)
                            (apigee--xml-append-step
                             (apigee--xml-flow-response postflow-elt) revision-header-policy)
                            (apigee--xml-append-step
                             default-faultrule-elt revision-header-policy))))
                      (if (seq-some #'identity changes)
                          (progn
                            (apigee--serialize-xml-elt root)
                            (save-buffer 0))
                        (message "[apigee] %s already has the revision logic"
                                 (file-name-nondirectory proxyendpoint-file)))))))))))))

(defun apigee--ensure-assign-message-policy (template-file policy-dir)
  "Create an AssignMessage policy from TEMPLATE-FILE in POLICY-DIR, if absent.
The policy name is derived from TEMPLATE-FILE. Return the policy name."
  (let* ((name-elements (apigee--policy-name-elements "AssignMessage" template-file))
         (policy-name (concat (car name-elements) "-" (cadr name-elements)))
         (fullpath-dest-policy-path
          (apigee--join-path-elements policy-dir (concat policy-name ".xml"))))
    (unless (file-exists-p fullpath-dest-policy-path)
      (let ((elaborated-contents
             (apigee--elaborate-template
              (apigee--join-path-elements apigee--base-template-dir "policies" "AssignMessage" template-file)
              policy-name)))
        ;; create the file, expand the snippet, save it.
        (with-current-buffer (find-file-noselect fullpath-dest-policy-path)
          ;; yas-expand-snippet-sync does not return until the snip is expanded.
          (yas-expand-snippet-sync elaborated-contents (point) (point))
          (save-buffer))))
    policy-name))

(defun apigee--xml-flow-response (flow)
  "Return the Response child of the xml.el node FLOW, adding one if missing."
  (or (car (xml-get-children flow 'Response))
      (let ((response (car (apigee--parse-xml "<Response></Response>"))))
        ;; insert as last element in the flow
        (setcdr (last flow) (list response))
        response)))

(defun apigee--xml-step-names (parent)
  "Return the policy names referenced by the Step children of xml.el node PARENT."
  (mapcar (lambda (step)
            (string-trim
             (or (car (xml-node-children (car (xml-get-children step 'Name)))) "")))
          (xml-get-children parent 'Step)))

(defun apigee--xml-append-step (parent policy-name)
  "Append a Step for POLICY-NAME to xml.el node PARENT, unless one is there.
Return non-nil if PARENT was changed."
  (unless (member policy-name (apigee--xml-step-names parent))
    (let ((step (car (apigee--parse-xml
                      (concat "  <Step>\n    <Name>" policy-name "</Name>\n  </Step>")))))
      (setcdr (last parent) (list "\n    " step))
      t)))


(defun apigee--xml-insert-child-before (node new-child before-names)
  "Insert NEW-CHILD into the xml.el NODE, before a child named in BEFORE-NAMES.
BEFORE-NAMES is a preference list of element-name symbols: the first name
that NODE has a child for decides where NEW-CHILD goes. If NODE has none
of them, NEW-CHILD becomes its last child."
  (let ((anchor-name (seq-find (lambda (name) (xml-get-children node name))
                               before-names))
        ;; Start at the attributes cell; the children follow it.
        (cell (cdr node)))
    (while (and (cdr cell)
                (not (and anchor-name
                          (consp (cadr cell))
                          (eq (xml-node-name (cadr cell)) anchor-name))))
      (setq cell (cdr cell)))
    (setcdr cell (cons new-child (cdr cell)))))

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
  (let ((bundle-dir (apigee--require-bundle))
        (bundle-type (apigee--type-of-bundle))
        (choice (apigee--prompt-user-with-policy-choices)))
    (when choice ;; is a cons cell
      (let ((policy-dir (concat bundle-dir bundle-type "/policies/"))
            (ptype (car choice))
            (template-filename (concat (cadr choice) ".xml"))
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
  (when-let* ((s)
              (bundle-root (apigee--root-path-of-bundle)))
    (and (file-exists-p s)
         ;; The bundle root is a truename, so compare against S's truename.
         (s-starts-with? (concat bundle-root (apigee--type-of-bundle) "/policies/")
                         (file-truename s)))))

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

(defun apigee--policy-name-char-p (char)
  "Return non-nil if CHAR can be part of a policy name.
Period is excluded so that flow variables like ratelimit.Quota-1.allowed
count as references to the policy Quota-1."
  (and char (string-match-p "[[:alnum:]_-]" (string char))))

(defun apigee--replace-policy-name-in-buffer (old-name new-name)
  "Replace each whole occurrence of policy name OLD-NAME with NEW-NAME.
An occurrence that is part of a longer name is left alone, so renaming
AM-Foo does not touch AM-Foo-1. Return non-nil if anything was replaced."
  (let ((case-fold-search nil)
        (replaced nil))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward old-name nil t)
        (unless (or (apigee--policy-name-char-p (char-before (match-beginning 0)))
                    (apigee--policy-name-char-p (char-after (match-end 0))))
          (replace-match new-name t t)
          (setq replaced t))))
    replaced))

(defun apigee-rename-policy ()
  "Rename a policy to a new name"
  (interactive)
  (let ((current-policies (apigee--list-policies))
        (predicate-ignored nil)
        (require-match nil)
        (initial-input (apigee--current-policy-name))
        (hist nil)
        (def (apigee--current-policy-name)))
    (let* ((name-of-policy-to-rename
            (completing-read "rename policy: "
                             (dc-ss/completion-fn current-policies 'sorted-sanely)
                             predicate-ignored require-match initial-input hist def))
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

              (let* ((interactive-buffer-name (buffer-name))
                     (buf-for-policy-file (get-file-buffer fq-old-fname))
                     (count-of-changes 0)
                     (process-one-file (lambda (one-file)
                                         (with-current-buffer (find-file-noselect one-file)
                                           (let ((original-modified (buffer-modified-p)))
                                             (when (apigee--replace-policy-name-in-buffer
                                                    name-of-policy-to-rename new-name)
                                               (setq count-of-changes (1+ count-of-changes))
                                               (unless (or original-modified
                                                           (s-equals-p interactive-buffer-name (buffer-name)))
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

(defconst apigee--editor-temp-file-globs '("#*#" "*~" ".#*")
  "Globs for Emacs auto-save, backup and lock files.
These must not be carried from a template into a new bundle.")

(defun apigee--copy-subdirs (subdirs source-dir dest-dir)
  "Copy each of SUBDIRS that exists in SOURCE-DIR into DEST-DIR.
Editor temp files, per `apigee--editor-temp-file-globs', are removed
from the copies."
  (dolist (subdir subdirs)
    (let ((source-subdir (apigee--join-path-elements source-dir subdir))
          (dest-subdir (apigee--join-path-elements dest-dir subdir)))
      (when (apigee--is-existing-directory source-subdir)
        (copy-directory source-subdir dest-subdir t nil)
        (apigee--clean-files-from-dirtree dest-subdir apigee--editor-temp-file-globs)))))

(defun apigee--clean-files-from-dirtree (directory globs)
  "Recursively delete files in DIRECTORY whose names match any of GLOBS."
  (let ((regexp (mapconcat #'wildcard-to-regexp globs "\\|")))
    (dolist (file (directory-files-recursively directory regexp))
      (delete-file file))))

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
    (apigee--copy-subdirs (list "sharedflows" "policies" "resources") source-sf-dir dest-sf-dir)))

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
and non-existent directories, in sorted order.  If WITH-TIME is non-nil,
emit the list with time (suitable for persisting). Otherwise without -
it will be a bare list of strings representing directory paths, suitable
for use within an `completing-read'."
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
           (completing-read
            "containing directory?: "
            (dc-ss/completion-fn
             (mapcar (lambda (x) (replace-regexp-in-string homedir "~/" x))
                     (delq nil (delete-dups candidate-list)))
             'sorted-sanely)
            nil nil nil)))
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
directory. If no directory has ever been used, it prompts for the
directory."
  (interactive "P")
  (apigee--new-asset "proxy" arg))


(defun apigee-new-sharedflow (arg)
  "Interactive fn that creates a new exploded sharedflow bundle directory
structure. Prompts for the name of the sharedflow, and the base template
for the thing.

When invoked with a prefix, this fn will prompt the user also for
the name of the directory in which to store the sharedflow.

When invoked without a prefix, it uses the most recent asset home
directory. If no directory has ever been used, it prompts for the
directory."
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
directory. If no directory has ever been used, it prompts for the
directory.

This fn is not intended to be called directly.

ASSET-TYPE - either \"proxy\" or \"sharedflow\"
PROMPT-ARG - whether invoked with a prefix

"
  (interactive "P")
  (let ((alist-sym (intern (format "apigee--%s-template-alist" asset-type))))
    (let ((asset-name (read-string (format "%s name?: " asset-type) nil nil nil))
          (template
           (completing-read
            (format "%s template: " asset-type)
            (dc-ss/completion-fn
             (mapcar (lambda (x) (car x)) (symbol-value alist-sym))
             'sorted-sanely)
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

     ;; 20250527-1829 - no longer using popup.
     ;; ;; override the impl in popup.el - it's broken for me for cascaded menus - 20230627-1541
     ;; (defun popup-child-point (popup &optional offset)
     ;;   (overlay-end
     ;;    (popup-line-overlay popup (popup-selected-line popup))))

     ;;      ;; customize the popup font
     ;; (set-face-attribute 'popup-menu-face nil :height 105)
     ;; (set-face-attribute 'popup-menu-selection-face nil :height 105)

     (apigee--restore-state)
     (if (not apigee--base-template-dir)
         (setq apigee--base-template-dir (concat (file-name-directory apigee--load-file-name) "templates/")))
     (if apigee--base-template-dir
         (apigee-load-templates apigee--base-template-dir))
     (setq apigee-timer
      (run-with-timer (* 60 apigee--timer-minutes) (* 60 apigee--timer-minutes) 'apigee--persist-state))))

(provide 'apigee)

;;; apigee.el ends here
