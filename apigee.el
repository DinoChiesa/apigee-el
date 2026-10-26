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
;; Package-Requires: ((emacs "28.1") (yasnippet "0.14.0"))
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

(require 'seq)
(require 'xml)
(require 'compile)
(require 'cl-lib) ; For cl-letf

;; Declared special so that `let' binds them dynamically even when this file
;; is byte-compiled, matching how they behave when it is loaded from source.
(defvar tramp-connection-properties)
(defvar tramp-default-remote-shell)
(defvar tramp-encoding-shell)
(defvar comint-last-output-start)

(declare-function yas-expand-snippet "yasnippet" (snippet &optional start end expand-env))
(declare-function yas-active-snippets "yasnippet" (&optional beg end))
(declare-function yas-minor-mode "yasnippet" (&optional arg))
(defvar yas-minor-mode)

(defgroup apigee nil
  "Create and work with Apigee API proxy and sharedflow bundles."
  :group 'tools
  :prefix "apigee-")

(define-obsolete-variable-alias 'apigee-function-cache 'apigee--function-cache "2026-10-26")
(defvar apigee--function-cache (make-hash-table :test 'equal)
  "Cache for memoizing functions with timeouts.")

(defvar apigee--base-template-dir nil
  "The directory from which policy templates have been most recently loaded")

(defvar apigee--recently-used-asset-homes nil
  "a list of directories recently used to store API Proxies.")

(define-obsolete-variable-alias 'apigee--verbose-logging 'apigee-verbose-logging "2026-10-26")
(defcustom apigee-verbose-logging nil
  "Whether apigee.el logs verbosely into *Messages*."
  :type 'boolean)

(define-obsolete-variable-alias 'apigee--timer-minutes 'apigee-timer-minutes "2026-10-26")
(defcustom apigee-timer-minutes 6
  "Interval in minutes between saves of the apigee.el settings file.
Takes effect when apigee.el is loaded."
  :type 'natnum)

(define-obsolete-variable-alias 'apigee-timer 'apigee--timer "2026-10-26")
(defvar apigee--timer nil
  "Cancellable timer, for saving apigee.el settings.")

(defvar apigee--list-of-vars-to-store-and-restore
  '(apigee-verbose-logging apigee--recently-used-asset-homes
    apigee--base-template-dir apigee-organization
    apigee-environment apigee-service-account)
  "Variables stored in, and restored from, the settings file.")

(defvar apigee--settings-file-base-name "apigee-el.dat")

(defcustom apigee-xmlschema-validator-home "Apigee-Policy-Schema"
  "The directory that holds the Apigee policy schema and validator tool.
If this is a relative directory, it will be resolved relative to the
directory containing the apigee.el file.
You need to have created the venv (in either .venv or venv) and installed
the prerequisites via `pip install -r requirements.txt`"
  :type 'directory)

(defcustom apigee-commands-alist
  '(
    (import . ("%apigeecli" "apis" "create" "bundle" "-f" "apiproxy" "--name" "%n" "-o" "%o" "--token" "%t"))
    (deploy . ("%apigeecli" "apis" "deploy" "--wait" "--name" "%n" "--ovr" "--org" "%o" "--env" "%e" "%sa_args" "--token" "%t"))
    (import-and-deploy . (import deploy))
    (sharedflow-import . ("%apigeecli" "sharedflows" "create" "bundle" "-f" "sharedflowbundle" "--name" "%n" "-o" "%o" "--token" "%t"))
    (sharedflow-deploy . ("%apigeecli" "sharedflows" "deploy" "--wait" "--name" "%n" "--ovr" "--org" "%o" "--env" "%e" "%sa_args" "--token" "%t"))
    (sharedflow-import-and-deploy . (sharedflow-import sharedflow-deploy))
    (lint . ("%apigeelint" "-s" "." "-e" "%lint-exclusions" "-f" "visualstudio.js"))
    (validate . ("%validate-cmd" "--source" "."))
    )
  "The commands run by `apigee-lint-asset', `apigee-import-proxy' and so on.
In a sharedflow, the import and deploy commands use the entries named
sharedflow-import, sharedflow-deploy and sharedflow-import-and-deploy.
Each value is one of:
- a list of words. A word is either literal, or %KEY where KEY is in
  `apigee-programs-alist' or `apigee-placeholders-alist'; such a word
  expands to zero or more words. Every word is shell-quoted.
- a list of command symbols, run in sequence with \"&&\", so a failure
  stops the rest.
- a string: a shell command line used as-is, except that %KEY is
  replaced with its (shell-quoted) expansion and %c with \"&&\"."
  :type '(alist :key-type (symbol :tag "Command")
                :value-type (choice (repeat :tag "Words" string)
                                    (repeat :tag "Sequence of commands" symbol)
                                    (string :tag "Shell command line"))))

;; To modify one of the above after program starts:
;; (setf (alist-get 'lint apigee-commands-alist)
;;      "%apigeelint -s . -e %lint-exclusions -f visualstudio.js")

(defcustom apigee-programs-alist
  '(
    (gcloud . "gcloud")
    (apigeecli . "apigeecli")
    (apigeelint . ("node" "~/path/to/apigeelint/cli.js"))
    (validate-cmd . apigee--get-xmlschema-validator-program)
    )
  "Programs used in `apigee-commands-alist', as %KEY.
Each value is a string, a list, or a function returning one of those. A
string is a program name or path (spaces in the path are fine), or a
program followed by arguments separated by spaces. A list is a program
and its arguments. A program without a directory is looked up with
`executable-find'. You can set the path explicitly in your emacs init
file, for example,
  (setf (alist-get 'apigeecli apigee-programs-alist) \"~/.apigeecli/bin/apigeecli\"
        (alist-get 'gcloud apigee-programs-alist) \"~/gcloud-sdk/bin/gcloud\"
    )"
  :type '(alist :key-type (symbol :tag "Program key")
                :value-type (choice (string :tag "Program (and arguments)")
                                    (repeat :tag "Program and arguments" string)
                                    (function :tag "Function returning a program"))))

(defvar apigee--gcloud-access-token-cache
  nil
  "Memoized `apigee--gcloud-print-access-token'; see `apigee--gcloud-access-token'.")

(defvar apigee-placeholders-alist
  `(
    (n               . apigee--bundle-name)
    (o               . apigee-get-organization)
    (e               . apigee-get-environment )
    (lint-exclusions . apigee-get-lint-exclusions )
    (sa_args         . apigee-get-service-account-args-for-deployment )
    (t               . apigee--gcloud-access-token)
    )
  "Placeholders used in `apigee-commands-alist', as %KEY.
Each value is a function called with WANT-PROMPT. It returns a string
(one word), a list of strings, or nil or \"\" (no words). The token, %t,
is filled in last, after the user has had a chance to edit the command."
  )

;; useful during development
;;(add-to-list 'apigee-placeholders-alist '(lint-exclusions . apigee-get-lint-exclusions))


(defconst apigee--compilation-error-symbols
  '(apigeelint-visualstudio-error-with-lines apigeelint-visualstudio-error-no-line)
  "Entries that `apigee-setup' adds to `compilation-error-regexp-alist'.")

(defun apigee--register-compilation-error-regexps ()
  "Teach compilation mode the apigeelint visualstudio output format."
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
                 2 3 nil 2 1 )))

;; to modify:
;; (setf
;;  (alist-get 'apigeelint-visualstudio-error-no-line compilation-error-regexp-alist-alist)
;;    '( "^\\(\\([^ \n]+\\)(\\(0\\)\\)): \\(.+\\)$"
;;     2 3 nil 2 1 ))

(defcustom apigee-organization nil
  "The organization to use for deployments. apigee.el will prompt if unset."
  :type '(choice (const :tag "Prompt when needed" nil) string))
(defcustom apigee-environment nil
  "The environment to use for deployments. apigee.el will prompt if unset."
  :type '(choice (const :tag "Prompt when needed" nil) string))
(defcustom apigee-service-account nil
  "The service account to use for deployments.
The format is {ACCOUNT_ID}@{PROJECT}.iam.gserviceaccount.com, or just
{ACCOUNT_ID}. \"-\" means deploy without a service account; nil means
prompt when needed."
  :type '(choice (const :tag "Prompt when needed" nil)
                 (const :tag "No service account" "-")
                 string))

(defvar apigee--load-file-name load-file-name
  "The name from which the Apigee module was loaded.")

(defconst apigee--policytype-shortform
  (list
   '("AssignMessage" "AM")
   '("AccessEntity" "AE")
   '("ServiceCallout" "SC")
   '("XSL" "XSL")
   '("AccessControl" "AC")
   '("RaiseFault" "RF")
   '("MessageLogging" "ML")
   '("FlowCallout" "FC")
   '("JavaScript" "JS")
   '("ExtractVariables" "EV")
   '("OAuthV2" "OAuthV2")
   '("XMLToJSON" "X2J")
   '("CORS" "CORS")
   ;; '("VerifyJWT" "JWT")
   ;; '("GenerateJWT" "JWT")
   ;; '("DecodeJWT" "JWT")
   '("JSONToXML" "J2X")
   '("BasicAuthentication" "BA"))
  "Abbreviations used as the prefix of suggested policy names.
Keyed by policy type, which is the name of a directory under
templates/policies. A type with no entry uses the directory name.")

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


(defcustom apigee-lint-exclusions "TD002,TD007,TD012"
  "The comma-separated list of plugins to exclude from apigeelint."
  :type 'string)

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

(defconst apigee--asset-kinds
  '(("proxy"
     :templates apigee--proxy-template-alist
     :template-subdir "proxies"
     :bundle "apiproxy"
     :subdirs ("proxies" "policies" "resources" "targets")
     :after-copy apigee--set-proxy-basepath
     :commands ((import . import) (deploy . deploy)
                (import-and-deploy . import-and-deploy)))
    ("sharedflow"
     :templates apigee--sharedflow-template-alist
     :template-subdir "sharedflows"
     :bundle "sharedflowbundle"
     :subdirs ("sharedflows" "policies" "resources")
     :commands ((import . sharedflow-import) (deploy . sharedflow-deploy)
                (import-and-deploy . sharedflow-import-and-deploy))))
  "What differs between the kinds of asset created from a template.
Each entry is (ASSET-TYPE . PLIST), with these properties:
  :templates        variable holding the loaded templates, an alist
  :template-subdir  directory under `apigee--base-template-dir' holding
                    the templates
  :bundle           name of the bundle directory, e.g. \"apiproxy\"
  :subdirs          bundle subdirectories copied from a template
  :after-copy       optional function called with the new bundle
                    directory and the asset name, after the copy
  :commands         alist mapping the actions import, deploy and
                    import-and-deploy to keys in `apigee-commands-alist'")

(defun apigee--asset-kind (asset-type property)
  "Return PROPERTY of ASSET-TYPE from `apigee--asset-kinds'."
  (plist-get (or (cdr (assoc asset-type apigee--asset-kinds))
                 (error "[apigee] unknown asset type %s" asset-type))
             property))


(defun apigee--blank-p (s)
  "Return non-nil if S is nil or the empty string."
  (or (null s) (string-empty-p s)))

(defun apigee--capitalize-first (s)
  "Return S with its first character upcased and the rest downcased."
  (if (string-empty-p s)
      s
    (concat (upcase (substring s 0 1)) (downcase (substring s 1)))))

(defun apigee--first-available-name (base available-p &optional start-at)
  "Return the first of BASE, BASE-1, BASE-2, ... that AVAILABLE-P accepts.
With START-AT, begin at BASE-START-AT instead of BASE."
  (let* ((n start-at)
         (name-for (lambda (n) (if n (format "%s-%d" base n) base))))
    (while (not (funcall available-p (funcall name-for n)))
      (setq n (1+ (or n 0))))
    (funcall name-for n)))

(defun apigee--read-unused-name (label default available-p)
  "Read a name for a LABEL, offering DEFAULT, until AVAILABLE-P accepts it."
  (let ((name (read-string (format "%s name: " (downcase label)) default nil default)))
    (while (not (funcall available-p name))
      (setq name (read-string (format "That name is in use. %s name: " label)
                              default nil default)))
    name))

(defun apigee--get-or-prompt (sym label want-prompt)
  "Return the value of SYM, prompting with LABEL if WANT-PROMPT or it is blank.
The current value is offered as the default. The result is stored in SYM."
  (let ((value (symbol-value sym)))
    (set sym (if (or want-prompt (apigee--blank-p value))
                 (apigee--read-or-default label value)
               value))))

(defun apigee--completion-table (candidates)
  "Return a completion table for CANDIDATES that lists them alphabetically.
The default minibuffer sort puts short candidates first. The table's
category is `sorted-sanely', so an entry for that category in
`completion-category-overrides' still takes precedence."
  (let ((sort-alphabetically (lambda (cands) (sort (copy-sequence cands) #'string<))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata (category . sorted-sanely)
                     (display-sort-function . ,sort-alphabetically)
                     (cycle-sort-function . ,sort-alphabetically))
        (complete-with-action action candidates string pred)))))

(defvar apigee--in-snippet-recursive-edit nil
  "Non-nil while `apigee--expand-snippet' waits for a snippet to be finished.")

(defun apigee--exit-snippet-recursive-edit ()
  "End the recursive edit started by `apigee--expand-snippet'.
For buffer-local use in `yas-after-exit-snippet-hook'."
  (when apigee--in-snippet-recursive-edit
    (throw 'exit nil)))

(defun apigee--expand-snippet (content)
  "Expand CONTENT as a yasnippet snippet at point, and return when it's done.
If the snippet has fields, wait in a recursive edit until the user has
filled them in and the snippet exits."
  (require 'yasnippet)
  ;; `yas-expand-snippet' refuses to run without the minor mode.
  (unless yas-minor-mode
    (yas-minor-mode 1))
  (add-hook 'yas-after-exit-snippet-hook #'apigee--exit-snippet-recursive-edit nil t)
  (yas-expand-snippet content (point) (point))
  (when (yas-active-snippets)
    (let ((apigee--in-snippet-recursive-edit t))
      (recursive-edit))))

(defun apigee--xml-parse-tag-1 (&optional parse-dtd parse-ns)
  "Like `xml-parse-tag-1', but keep XML comments as (nil . \"<!-- ... -->\") nodes.
Installed only temporarily, by `apigee--with-comment-preserving-xml'."
  (let* ((xml-validating-parser (or parse-dtd xml-validating-parser))
         (xml-ns
          (cond ((eq parse-ns 'symbol-qnames)
                 (cons 'symbol-qnames xml-default-ns))
                ((or (consp (car-safe parse-ns))
                     (and (eq (car-safe parse-ns) 'symbol-qnames)
                          (listp (cdr parse-ns))))
                 parse-ns)
                (parse-ns
                 xml-default-ns))))
    (cond
     ;; Processing instructions
     ((looking-at-p "<\\?")
      (search-forward "?>")
      (skip-syntax-forward " ")
      (apigee--xml-parse-tag-1 parse-dtd xml-ns))
     ;; CDATA
     ((looking-at "<!\\[CDATA\\[")
      (let ((pos (match-end 0)))
        (unless (search-forward "]]>" nil t)
          (error "XML: (Not Well Formed) CDATA section does not end anywhere in the document"))
        (concat
         (buffer-substring-no-properties pos (match-beginning 0))
         (xml-parse-string))))
     ;; DTD
     ((looking-at-p "<!DOCTYPE[ \t\n\r]")
      (let ((dtd (xml-parse-dtd parse-ns)))
        (skip-syntax-forward " ")
        (if xml-validating-parser
            (cons dtd (apigee--xml-parse-tag-1 nil xml-ns))
          (apigee--xml-parse-tag-1 nil xml-ns))))
     ;; comment - preserved
     ((looking-at-p "<!--")
      (let ((start (point)))
        (search-forward "-->")
        (prog1 (cons nil (buffer-substring-no-properties start (point)))
          (skip-syntax-forward " "))))
     ;; end tag
     ((looking-at-p "</")
      '())
     ;; opening tag
     ((looking-at (concat "<\\(" xml-name-re "\\)"))
      (goto-char (match-end 1))
      (let* ((node-name (match-string-no-properties 1))
             (attrs (xml-parse-attlist xml-ns))
             children)
        (when (consp xml-ns)
          (dolist (attr attrs)
            (when (and (consp (car attr))
                       (equal "http://www.w3.org/2000/xmlns/" (caar attr)))
              (push (cons (cdar attr) (cdr attr))
                    (if (symbolp (car xml-ns))
                        (cdr xml-ns)
                      xml-ns)))))
        (setq children (list attrs (xml-maybe-do-ns node-name "" xml-ns)))
        (cond
         ((looking-at-p "/>")
          (forward-char 2)
          (nreverse children))
         ((eq (char-after) ?>)
          (forward-char 1)
          (let ((end (concat "</" node-name "\\s-*>")))
            (while (not (looking-at end))
              (cond
               ((eobp)
                (error "XML: (Not Well-Formed) End of document while reading element `%s'" node-name))
               ((looking-at-p "</")
                (forward-char 2)
                (error "XML: (Not Well-Formed) Invalid end tag `%s' (expecting `%s')"
                       (let ((pos (point)))
                         (buffer-substring pos (if (re-search-forward "\\s-*>" nil t)
                                                   (match-beginning 0)
                                                 (point-max))))
                       node-name))
               ((= (char-after) ?<)
                (let ((tag (apigee--xml-parse-tag-1 nil xml-ns)))
                  (when tag
                    (push tag children))))
               (t
                (let ((expansion (xml-parse-string)))
                  (push (if (stringp (car children))
                            (concat (pop children) expansion)
                          expansion)
                        children))))))
          (goto-char (match-end 0))
          (nreverse children))
         (t
          (error "XML: (Well-Formed) Couldn't parse tag: %s"
                 (buffer-substring-no-properties (- (point) 10) (+ (point) 1)))))))
     (t
      (unless xml-sub-parser
        (error "XML: (Well-Formed) Invalid character"))
      (let ((s (xml-parse-string)))
        (when (zerop (length s))
          (error "XML: (Not Well-Formed) Could not parse: %s"
                 (buffer-substring-no-properties (point) (min (+ (point) 10) (point-max)))))
        s)))))

(defun apigee--xml-print-internal (xml indent-string)
  "Like `xml-debug-print-internal', but print (nil . COMMENT) nodes as-is.
Installed only temporarily, by `apigee--with-comment-preserving-xml'."
  (let ((tree xml)
        attlist)
    (insert indent-string ?< (symbol-name (xml-node-name tree)))
    (setq attlist (xml-node-attributes tree))
    (while attlist
      (insert ?\s (symbol-name (caar attlist)) "=\""
              (xml-escape-string (cdar attlist)) ?\")
      (setq attlist (cdr attlist)))
    (setq tree (xml-node-children tree))
    (if (null tree)
        (insert ?/ ?>)
      (insert ?>)
      (dolist (node tree)
        (cond
         ((listp node)
          (cond
           ((not (car node))
            (insert (cdr node)))
           (t
            (insert ?\n)
            (apigee--xml-print-internal node (concat indent-string "  ")))))
         ((stringp node)
          (insert (xml-escape-string node)))
         (t
          (error "Invalid XML tree"))))
      (when (not (and (null (cdr tree)) (stringp (car tree))))
        (insert ?\n indent-string))
      (insert ?< ?/ (symbol-name (xml-node-name xml)) ?>))))

(defmacro apigee--with-comment-preserving-xml (&rest body)
  "Run BODY with xml.el parsing and printing that keep XML comments.
Unlike a global redefinition, this affects only apigee's own XML calls."
  `(cl-letf (((symbol-function 'xml-parse-tag-1) #'apigee--xml-parse-tag-1)
             ((symbol-function 'xml-debug-print-internal) #'apigee--xml-print-internal))
     ,@body))

(defun apigee--parse-buffer ()
  "Parse the current buffer as XML, keeping comments."
  (apigee--with-comment-preserving-xml
   (xml-parse-region (point-min) (point-max))))

(defun apigee-get-lint-exclusions (&optional want-prompt)
  "get a string representing a comma-separated set of plugins to
exclude from apigeelint. Uses a cached value, or, if WANT-PROMPT, will
prompt the user."
  (interactive "P")
  (apigee-setup)
  (apigee--get-or-prompt 'apigee-lint-exclusions "exclusions" want-prompt))


(defun apigee--path-to-settings-file ()
  "a function rturning the path to the settings file for apigee.el"
  (apigee--join-path-elements user-emacs-directory apigee--settings-file-base-name))

(defun apigee--restore-state ()
  "function expected to be called on initial module load, that restores the
previous state of the module. Things like the most recently used
apiproxy home, or the most recently loaded templates directory.

A variable that already has a non-nil value, for example one set in the
init file, keeps it; only unset variables are restored."
  (let ((dat-file-path (apigee--path-to-settings-file)))
    (if (file-exists-p dat-file-path)
        (with-temp-buffer
          (insert-file-contents dat-file-path)
          (save-excursion
            (goto-char (point-min))
            (let ((settings-data (read (current-buffer))))
              (dolist (one-setting settings-data)
                ;; Older files name the variables with strings, and some
                ;; under names that are now obsolete aliases.
                (let* ((name (car one-setting))
                       (sym (indirect-variable (if (stringp name) (intern name) name)))
                       (value (cadr one-setting)))
                  (if (and
                       (memq sym apigee--list-of-vars-to-store-and-restore)
                       value
                       (not (symbol-value sym)))
                      (set sym value)))))))
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
    (dolist (sym apigee--list-of-vars-to-store-and-restore)
      (push (list sym (symbol-value sym)) alist-to-write))
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
                        (not (string-suffix-p suffix clean-name)))
                   (string-match "^\\." clean-name)
                   (string-match "^#.*#$" clean-name)
                   (string-match "~$" clean-name)
                   (file-directory-p file))))
              (directory-files containing-dir t)))

(defun apigee--sort-by-string-car (list-o-lists)

  "sort LIST-O-LISTS, a list of lists, each of the form (STRING (LIST)),
lexicographically by the car of each element, which is a string."
  (sort list-o-lists
        (lambda (a b) (string< (car a) (car b) ))))

(defun apigee--get-target-template-filename (template-filename)
  "return the fullpath name of the target template file.
"
  (if (string-prefix-p "/" template-filename)
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
  (list (file-name-nondirectory policy-type-directory)
        (sort (mapcar #'file-name-nondirectory
                      (apigee--proper-files policy-type-directory ".xml"))
              #'string<)))

(defun apigee--policy-template-count ()
  "returns the total number of policy templates available in
`apigee--policy-template-alist'"
  (apply '+
         (mapcar (lambda (item) (length (cadr item)))
                 apigee--policy-template-alist)))

(defun apigee--load-template-alist (sym label subdir list-entries make-entry &optional count)
  "Set SYM to the template alist built from SUBDIR of the template dir.
LIST-ENTRIES is called with the directory and returns the entries (files
or directories); MAKE-ENTRY turns each into an alist element (NAME ...).
The alist is sorted by name. Return (LABEL N DIR), where N is the number
of templates: the length of the alist, or what COUNT returns for it."
  (let* ((top-level-dir (apigee--join-path-elements apigee--base-template-dir subdir))
         (alist (apigee--sort-by-string-car
                 (mapcar (lambda (entry)
                           (and apigee-verbose-logging (message "%s template %s" label entry))
                           (funcall make-entry entry))
                         (funcall list-entries top-level-dir)))))
    (set sym alist)
    (list label (funcall (or count #'length) alist) top-level-dir)))

(defun apigee--load-asset-templates (asset-type)
  "Load the templates for ASSET-TYPE, a key in `apigee--asset-kinds',
from the template dir, then set the kind's :templates variable to the alist."
  (apigee--load-template-alist
   (apigee--asset-kind asset-type :templates) asset-type
   (apigee--asset-kind asset-type :template-subdir)
   #'apigee--proper-subdirs
   (lambda (dir) (list (file-name-nondirectory dir) dir))))

(defun apigee-load-proxy-templates ()
  "Load proxy templates from the proxy template dir. "
  (apigee--load-asset-templates "proxy"))

(defun apigee-load-sharedflow-templates ()
  "Load templates for sharedflows from the template dir. "
  (apigee--load-asset-templates "sharedflow"))

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
  (apigee--load-template-alist
   'apigee--policy-template-alist "policy" "policies"
   #'apigee--proper-subdirs
   #'apigee--templates-for-one-policy-type
   (lambda (_) (apigee--policy-template-count))))


(defun apigee-load-target-templates ()
  "Load target templates from the target template dir.

Each child under the template directory should be a .xml file defining a
target. The name of the file will be the name of the template in the
resulting menu."

  (apigee--load-template-alist
   'apigee--target-template-alist "target" "targets"
   (lambda (dir) (apigee--proper-files dir ".xml"))
   (lambda (file) (list (file-name-base file) file))))


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
         (if (apigee--blank-p default-val)
             (read-string (format "%s: " prompt-label))
           (read-string (format "%s (%s): " prompt-label default-val)
                        default-val))))
    (if (string-empty-p response-val)
        (and (not (apigee--blank-p default-val)) default-val)
      response-val)))

(defun apigee-get-organization (&optional want-prompt)
  "Returns the org to use for import and deployment. Uses a cached
value, or, if WANT-PROMPT, will prompt the user."
  (interactive "P")
  (apigee-setup)
  (let ((org (apigee--get-or-prompt 'apigee-organization "organization name" want-prompt)))
    (when (apigee--blank-p org)
      (user-error "[apigee] An organization name is required"))
    org))

(defun apigee-get-environment (&optional want-prompt)
  "Returns the environment to use for deployment. Uses a cached value,
or, if WANT-PROMPT, will prompt the user."
  (interactive "P")
  (apigee-setup)
  (let ((env (apigee--get-or-prompt 'apigee-environment "environment name" want-prompt)))
    (when (apigee--blank-p env)
      (user-error "[apigee] An environment name is required"))
    env))

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

Returns the arguments (\"--sa\" EMAIL), or nil for no service account."
  (interactive "P")
  (apigee-setup)
  (let ((sa-local (apigee--get-or-prompt 'apigee-service-account "service account" want-prompt))
        (no-sa-p (lambda (sa) (or (apigee--blank-p sa) (equal sa "-")))))
    (when (and (not (funcall no-sa-p sa-local)) (not (string-search "@" sa-local)))
      ;; user provided short form; append the boilerplate suffix.
      (setq sa-local (format "%s@%s.iam.gserviceaccount.com" sa-local (apigee-get-organization) )))
    (setq apigee-service-account sa-local) ;; for next time, and for persistence
    (unless (funcall no-sa-p sa-local)
      (list "--sa" sa-local))))

(defun apigee--trim-xml-suffix (s)
  "trims the .xml suffix from a template file name"
  (string-remove-suffix ".xml" s))

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
    (if (file-directory-p right-here)
        (list (file-name-directory default-directory) flavor)
      (let ((elts (reverse (split-string (file-name-directory default-directory) "/")))
            r)
        (while (and elts (not r))
          (if (string= (car elts) flavor)
              (setq r (reverse (cdr elts)))
            (setq elts (cdr elts))))
        (if r
            (list (mapconcat 'identity r "/") flavor ))))))

(defun apigee--bundle ()
  "Return the bundle containing `default-directory', or nil if there is none.
The value is a plist (:root ROOT :type TYPE). TYPE is \"apiproxy\" or
\"sharedflowbundle\". ROOT is the truename of the directory that contains
the bundle directory.

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
then ROOT is ~/foo/bar/APINAME/ (expanded). It always ends in slash."
  (when-let* ((found (seq-some #'apigee--find-bundle '("apiproxy" "sharedflowbundle"))))
    (list :root (file-truename (file-name-as-directory (car found)))
          :type (cadr found))))

(defun apigee--bundle-subdir (bundle &rest subdirs)
  "Return directory SUBDIRS of BUNDLE's bundle directory, ending in slash.
With no SUBDIRS, return the bundle directory itself, e.g. ROOT/apiproxy/."
  (file-name-as-directory
   (apply #'file-name-concat (plist-get bundle :root) (plist-get bundle :type) subdirs)))

(defun apigee--require-bundle ()
  "Return the current bundle, as from `apigee--bundle', or signal a `user-error'."
  (or (apigee--bundle)
      (user-error "[apigee] You do not appear to be in an API proxy or sharedflow directory")))

(defun apigee--target-name-is-available (targets-dir tname)
  "Return non-nil if no target named TNAME exists in TARGETS-DIR."
  (not (file-exists-p (file-name-concat targets-dir (concat tname ".xml")))))

(defun apigee--suggested-target-name (targets-dir template-name)
  "suggest a name for a target given a TEMPLATE-NAME for the target.
Based on file availability in TARGETS-DIR."
  (apigee--first-available-name
   template-name (lambda (name) (apigee--target-name-is-available targets-dir name)) 1))

;;;###autoload
(defun apigee-add-target ()
  "Invoke this interactively, and the fn will prompt the user to
choose a target type to insert."
  (interactive)
  (apigee-setup)
  (apigee--require-apiproxy "Adding a target")
  (let ((bundle (apigee--require-bundle))
        (template-name
         (completing-read
          "target template: "
          (apigee--completion-table
           (mapcar (lambda (x) (car x)) apigee--target-template-alist))
          nil t nil)))
    (progn
      (when (string-empty-p template-name)
        (user-error "[apigee] No target template chosen"))
      (let ((target-dir (apigee--bundle-subdir bundle "targets"))
            (choice (assoc template-name apigee--target-template-alist)))
        (let* ((template-filename (nth 1 choice))
               (source-target-template-filename
                (apigee--get-target-template-filename template-filename)))
          (and (not (file-exists-p target-dir))
               (make-directory target-dir))

          ;;(asset-name (read-string (format "%s name?: " asset-type) default-value nil default-value)))

          (let* ((default-value (apigee--suggested-target-name target-dir template-name))
                 (target-name
                  (apigee--read-unused-name
                   "Target" default-value
                   (lambda (name) (apigee--target-name-is-available target-dir name))))
                 (elaborated-contents
                  (apigee--elaborate-template
                   ;; does this need to be qualified as fullpath?
                   source-target-template-filename target-name)))

            ;; create the file, expand the snippet, save it.
            (find-file (concat target-dir target-name ".xml"))
            ;; apigee--expand-snippet does not return until the snip is expanded.
            (apigee--expand-snippet elaborated-contents)
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
                                               (apigee--completion-table effective-options)
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

(defun apigee--policy-name-is-available (policies-dir pname)
  "Return true if the passed policy name PNAME is unused, in other words
if no file exists by that name in POLICIES-DIR."
  (not (file-exists-p (file-name-concat policies-dir (concat pname ".xml")))))

(defun apigee--string-is-uppercase-acronym-p (str)
  (let ((case-fold-search nil))
    (string-match-p "\\`[-A-Z0-9]*\\'" str)))

(defun apigee--maybe-capitalize (str)
  "Capitalizes the string STR unless the string is an acronym like HS256 or PBES2."
  (if (apigee--string-is-uppercase-acronym-p str) str (apigee--capitalize-first str)))

(defun apigee--policy-name-elements (ptype policy-template-filename)
  "Returns a list containing name elements, (PREFIX SUFFIX), for a policy file."
  (let* ((basename (file-name-sans-extension (file-name-nondirectory policy-template-filename)))
         (name-elements (split-string basename " "))
         (is-jwt (or (string= ptype "JWT") (string= ptype "JWS")))
         (name-suffix (mapconcat 'apigee--maybe-capitalize
                                 (if is-jwt (seq-drop name-elements 1) name-elements)
                                 "-"))
         (name-prefix (if is-jwt
                          (concat (apigee--capitalize-first (car name-elements)) ptype)
                        (or (cadr (assoc ptype apigee--policytype-shortform)) ptype))))
    (list name-prefix name-suffix)))

(defun apigee--suggested-policy-name (policies-dir ptype filename)
  "Returns a string that contains a default policy name. Derives the name
from PTYPE policy type, and FILENAME which is the fully-qualified filename.
Uses a counter, so the name is unused in POLICIES-DIR.
"
  (let* ((name-elements (apigee--policy-name-elements ptype filename))
         (name-prefix (car name-elements))
         (name-suffix (cadr name-elements)))
    (apigee--first-available-name
     (concat name-prefix "-" name-suffix)
     (lambda (name) (apigee--policy-name-is-available policies-dir name)))))

(defun apigee--gcloud-print-access-token ()
  "Return the output of gcloud auth print-access-token, uncached."
  (let* ((gcloud-pgm (alist-get 'gcloud apigee-programs-alist))
         (words (or (apigee--resolve-program gcloud-pgm)
                    (error "cannot resolve %s" gcloud-pgm))))
    (car (last (apply #'process-lines (car words)
                      (append (cdr words) '("auth" "print-access-token")))))))

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
             (entry (gethash key apigee--function-cache))
             (now (current-time)))
        (if (and entry
                 (<= (float-time (time-subtract now (cdr entry))) timeout))
            (car entry) ; Return cached value
          (let ((result (apply lexically-bound-fn args)))
            (puthash key (cons result now) apigee--function-cache) ; Update cache
            result))))))

(define-obsolete-function-alias 'apigee-gcloud-auth-print-access-token
  #'apigee--gcloud-print-access-token "2026-10-26")

(setq apigee--gcloud-access-token-cache
      (apigee--memoize-with-timeout #'apigee--gcloud-print-access-token (* 1 60)))

(defun apigee--gcloud-access-token (&rest _)
  "Return a gcloud access token, cached for a minute.
The %t placeholder in `apigee-placeholders-alist'."
  (funcall apigee--gcloud-access-token-cache))


(defun apigee--get-xmlschema-validator-program ()
  "Resolves the command for the XML Schema validator program.
It constructs the path to the Python executable within the virtual
environment and the ='validateBundle.py'= script, relative to
='apigee-xmlschema-validator-home'=."
  (if-let* ((apigee-el-dir (file-name-directory apigee--load-file-name))
            (base-dir (if (file-name-absolute-p apigee-xmlschema-validator-home)
                          apigee-xmlschema-validator-home
                        (file-truename (apigee--join-path-elements apigee-el-dir apigee-xmlschema-validator-home))))
            (_ (file-directory-p base-dir))
            (py-dir (apigee--join-path-elements base-dir "py"))
            (_ (file-directory-p py-dir))
            (venv-name (seq-find
                        (lambda (v)
                          (file-directory-p (apigee--join-path-elements py-dir v)))
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
            (list python-path validate-script)
          (error "Python executable not found at %s" python-path)))
    (error "Could not resolve XML Schema validator program. Check 'apigee-xmlschema-validator-home'")))

(defun apigee--bundle-name (&rest _)
  "Return the short name of the current API proxy or sharedflow.
The name comes from the single top-level XML file in the bundle
directory. It accepts arguments because placeholder functions are
called with WANT-PROMPT."
  (when-let* ((bundle (apigee--bundle)))
    (let* ((bundle-dir (apigee--bundle-subdir bundle))
           (file-list (apigee--proper-files bundle-dir ".xml"))
           (defn-file (apigee--verify-exactly-one file-list bundle-dir)))
      (apigee--trim-xml-suffix (file-name-nondirectory defn-file)))))

(defun apigee--require-apiproxy (action)
  "Signal a `user-error' if the current bundle is a sharedflow.
ACTION names what the user tried, for the message, e.g. adding a target,
which sharedflows don't have."
  (when (equal (plist-get (apigee--bundle) :type) "sharedflowbundle")
    (user-error "[apigee] %s supports only API proxies, not sharedflows" action)))

(defun apigee--substitute-placeholder (key value template)
  "Replace each %KEY in TEMPLATE with VALUE, taken literally.
%KEY must not be followed by a name character, so %t does not match the
start of %token."
  (replace-regexp-in-string
   (concat "\\(%" (regexp-quote (symbol-name key)) "\\)\\(?:[^[:alnum:]_-]\\|\\'\\)")
   value template t t 1))

(defun apigee--find-executable (name)
  "Return the executable NAME as an absolute path, or nil.
A NAME with a directory part is checked directly; otherwise it is looked
up with `executable-find'."
  (when-let* ((path (if (file-name-directory name)
                        (let ((expanded (expand-file-name name)))
                          (and (file-executable-p expanded)
                               (not (file-directory-p expanded))
                               expanded))
                      (executable-find name))))
    path))

(defun apigee--resolve-program (pgm-arg)
  "Return the words that run PGM-ARG, or nil if its executable isn't found.
PGM-ARG is a value from `apigee-programs-alist': a function returning a
program, a list (PROGRAM ARGS...), or a string. A string is first tried
whole as an executable, so a path with spaces works; failing that, its
first word is the executable and the rest are arguments. The executable
becomes an absolute path, and arguments starting with ~ are expanded."
  (let ((with-args (lambda (program args)
                     (when-let* ((executable (and program (apigee--find-executable program))))
                       (cons executable
                             (mapcar (lambda (arg) (if (string-prefix-p "~" arg) (expand-file-name arg) arg))
                                     args))))))
    (cond
     ((functionp pgm-arg)
      (apigee--resolve-program (funcall pgm-arg)))
     ((consp pgm-arg)
      (funcall with-args (car pgm-arg) (cdr pgm-arg)))
     ((stringp pgm-arg)
      (or (funcall with-args pgm-arg nil)
          (let ((words (split-string pgm-arg " " t)))
            (funcall with-args (car words) (cdr words))))))))

(defun apigee--expand-word (word want-prompt)
  "Return the list of words that WORD expands to.
A word %KEY expands through `apigee-programs-alist' or
`apigee-placeholders-alist'; any other word stands for itself."
  (let ((key (and (string-prefix-p "%" word) (intern (substring word 1)))))
    (cond
     ((and key (assq key apigee-programs-alist))
      (let ((program (alist-get key apigee-programs-alist)))
        (or (apigee--resolve-program program)
            (error "cannot resolve program '%s'" program))))
     ((and key (assq key apigee-placeholders-alist))
      (let ((value (funcall (alist-get key apigee-placeholders-alist) want-prompt)))
        (cond ((stringp value) (and (not (string-empty-p value)) (list value)))
              (t value))))
     (t (list word)))))

(defun apigee--words-to-shell (words)
  "Join WORDS into a shell command line, quoting each word.
The keywords :and and :token stand for \"&&\" and the unexpanded %t."
  (mapconcat (lambda (word)
               (pcase word
                 (:and "&&")
                 (:token "%t")
                 (_ (shell-quote-argument word))))
             words " "))

(defun apigee--command-words (words want-prompt defer-token)
  "Expand the placeholders in the command template WORDS.
With DEFER-TOKEN, leave the token placeholder %t unexpanded, as :token."
  (mapcan (lambda (word)
            (cond ((member word '("&&" "%c")) (list :and))
                  ((and defer-token (equal word "%t")) (list :token))
                  (t (copy-sequence (apigee--expand-word word want-prompt)))))
          words))

(defun apigee--expand-command-string (template want-prompt defer-token)
  "Expand the %KEY placeholders in the shell command line TEMPLATE.
Each expansion is shell-quoted; %c becomes \"&&\". With DEFER-TOKEN,
%t is left as it is."
  (let ((command template))
    (dolist (key (append (mapcar #'car apigee-programs-alist)
                         (mapcar #'car apigee-placeholders-alist)))
      (when (and (not (and defer-token (eq key 't)))
                 (string-match-p (concat "%" (regexp-quote (symbol-name key))
                                         "\\(?:[^[:alnum:]_-]\\|\\'\\)")
                                 command))
        (setq command (apigee--substitute-placeholder
                       key
                       (apigee--words-to-shell
                        (apigee--expand-word (concat "%" (symbol-name key)) want-prompt))
                       command))))
    (apigee--substitute-placeholder 'c "&&" command)))

(defun apigee--get-command (symbol want-prompt &optional defer-token)
  "Return the shell command line for SYMBOL in `apigee-commands-alist'.
WANT-PROMPT is passed to the placeholder functions. With DEFER-TOKEN,
the token placeholder %t is left in the result, for
`apigee--insert-token' to fill in later."
  (let ((template (alist-get symbol apigee-commands-alist)))
    (cond
     ((null template) nil)
     ((stringp template)
      (apigee--expand-command-string template want-prompt defer-token))
     ((symbolp (car template))
      (mapconcat (lambda (command) (apigee--get-command command want-prompt defer-token))
                 template " && "))
     (t
      (apigee--words-to-shell (apigee--command-words template want-prompt defer-token))))))

(defun apigee--insert-token (command)
  "Replace the token placeholder %t in COMMAND with a gcloud access token."
  (if (string-match-p "%t\\(?:[^[:alnum:]_-]\\|\\'\\)" command)
      (apigee--substitute-placeholder 't (shell-quote-argument (apigee--gcloud-access-token)) command)
    command))

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

(defun apigee--has-same-bundle-root (root-dir-in-question)
  "predicate called in each unsaved buffer.  Returns t if the buffer
is part of an apiproxy, and if the ROOT-DIR-IN-QUESTION is the same
as the root of the current buffer.
Used as a predicate for `save-some-buffers'."
  (let ((bundle-dir (plist-get (apigee--bundle) :root)))
    (and bundle-dir
         (not (apigee--blank-p bundle-dir))
         (equal bundle-dir root-dir-in-question))))

(defun apigee--run-command-for-proxy (label command-symbol &optional want-prompt)
  "Run a command for the current API proxy or sharedflow."
  (apigee-setup)
  (if-let* ((bundle-dir (plist-get (apigee--bundle) :root)))
      (if-let* ((bundle-name (apigee--bundle-name))
                ;; When the user will edit the command, the token is filled in
                ;; afterwards, so it never appears in the minibuffer.
                (cmd (apigee--get-command command-symbol want-prompt want-prompt)))
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
                          (seq-find (lambda (entry)
                                      (and (equal (car entry) safe-ssh-spec-pattern)
                                           (equal (cadr entry) "remote-shell")))
                                    tramp-connection-properties)))
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
                           ;; The token is still the placeholder %t at this point.
                           (if want-prompt
                               (apigee--insert-token
                                (read-shell-command "Run build command: " cmd))
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
    (user-error "[apigee] You do not appear to be in an API proxy or sharedflow directory")))


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

(defun apigee-lint-asset (want-prompt)
  "Run apigeelint on the current API proxy or sharedflow.

With a prefix argument (e.g., C-u), prompt the user to edit
the command before running it."
  (interactive "P")
  (apigee--run-command-for-proxy "apigeelint" 'lint want-prompt))

(defun apigee-validate-xml-for-asset (want-prompt)
  "Run the XML Validator on the current API proxy or sharedflow.

With a prefix argument (e.g., C-u), prompt the user to edit
the command before running it."
  (interactive "P")
  (apigee--run-command-for-proxy "validate" 'validate want-prompt))

;;;###autoload
(defun apigee--bundle-command (action)
  "Return the `apigee-commands-alist' key for ACTION in the current bundle.
ACTION is import, deploy or import-and-deploy; the asset kind's
:commands in `apigee--asset-kinds' decide. Outside a bundle, return
ACTION, and let the command report that."
  (let* ((type (plist-get (apigee--bundle) :type))
         (kind (car (seq-find (lambda (k) (equal (plist-get (cdr k) :bundle) type))
                              apigee--asset-kinds))))
    (if kind
        (alist-get action (apigee--asset-kind kind :commands))
      action)))

(defun apigee-import-and-deploy-proxy (want-prompt)
  "Import AND deploy the API proxy or sharedflow.

With a prefix argument (e.g., C-u), prompt the user for org/env
before running it."
  (interactive "P") ;; if want-prompt is non-nil, prompt the user for org/env
  (apigee--run-command-for-proxy "apigeecli" (apigee--bundle-command 'import-and-deploy) want-prompt))

;;;###autoload
(defun apigee-import-proxy (want-prompt)
  "Import the API proxy or sharedflow."
  (interactive "P") ;; if want-prompt is non-nil, prompt the user for org/env
  (apigee--run-command-for-proxy "apigeecli" (apigee--bundle-command 'import) want-prompt))

;;;###autoload
(defun apigee-deploy-proxy (want-prompt)
  "Deploy the API proxy or sharedflow. This doesn't use the configuration in the
filesystem, beyond the name. It's assumed you've recently called
`apigee-import-proxy', and there is a revision to deploy."
  (interactive "P") ;; if want-prompt is non-nil, prompt the user for org/env
  (apigee--run-command-for-proxy "apigeecli" (apigee--bundle-command 'deploy) want-prompt))

;;;###autoload
(defun apigee-inject-proxy-revision-logic ()
  "Inject the policies and flow to insert a proxy revision header
into the response. Handy when modifying an existing proxy that
does not send back a revision header, or when creating a proxy
from a template that doesn't send back a revision header.

Running it again on a proxy that already has the logic changes nothing."
  (interactive)
  (apigee-setup)
  (apigee--require-apiproxy "Injecting the revision logic")
  (let* ((bundle (apigee--require-bundle))
         (bundle-type (plist-get bundle :type)))
    (if (equal bundle-type "apiproxy")
        (let* ((bundle-policy-dir (apigee--bundle-subdir bundle "policies"))
               (clean-headers-policy
                (apigee--ensure-assign-message-policy
                 "clean request headers from response.xml" bundle-policy-dir))
               (revision-header-policy
                (apigee--ensure-assign-message-policy
                 "inject proxy revision header.xml" bundle-policy-dir)))

          ;; and now inject the references to these policies in the
          ;; right places in the various proxy endpoint(s).

          (let ((bundle-proxy-dir
                 (apigee--bundle-subdir bundle "proxies")))
            (dolist (proxyendpoint-file
                     (apigee--proper-files bundle-proxy-dir ".xml"))
              (save-excursion
                (with-current-buffer (find-file-noselect proxyendpoint-file)
                  (let*
                      ((root (apigee--parse-buffer))
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
          ;; apigee--expand-snippet does not return until the snip is expanded.
          (apigee--expand-snippet elaborated-contents)
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
  "Parse XML-STRING, keeping comments; return a list of xml.el nodes."
  (with-temp-buffer
    (insert xml-string)
    (apigee--with-comment-preserving-xml
     (xml--parse-buffer nil nil))))


(defun apigee--elaborate-template (fullpath-template-filename thing-name)
  "returns the contents of the named template with the ## placeholders
replaced."
  (string-replace "##" thing-name (apigee--get-template-contents fullpath-template-filename)))


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
  (apigee-setup)
  (let* ((bundle (apigee--require-bundle))
         (bundle-dir (plist-get bundle :root))
         (bundle-type (plist-get bundle :type))
         (choice (apigee--prompt-user-with-policy-choices)))
    (when choice ;; is a cons cell
      (let ((policy-dir (apigee--bundle-subdir bundle "policies"))
            (ptype (car choice))
            (template-filename (concat (cadr choice) ".xml")))
        (and (not (file-exists-p policy-dir))
             (make-directory policy-dir))
        (let* ((default-value (apigee--suggested-policy-name policy-dir ptype template-filename))
               (policy-name
                (apigee--read-unused-name
                 "Policy" default-value
                 (lambda (name) (apigee--policy-name-is-available policy-dir name))))
               (fullpath-template-filename
                (apigee--join-path-elements apigee--base-template-dir "policies" ptype template-filename))
               (elaborated-contents
                (apigee--elaborate-template fullpath-template-filename policy-name)))

          ;; create the file, expand the snippet, save it.
          (find-file (concat policy-dir policy-name ".xml"))
          ;; apigee--expand-snippet does not return until the snip is expanded.
          (apigee--expand-snippet elaborated-contents)
          (save-buffer)
          ;;(apigee-mode 1)
          ;; here, optionally open the resource file, if any
          (cond
           ((member ptype '("JavaScript" "XSL"))
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
              (bundle (apigee--bundle)))
    (and (file-exists-p s)
         ;; The bundle root is a truename, so compare against S's truename.
         (string-prefix-p (apigee--bundle-subdir bundle "policies")
                         (file-truename s)))))

(defun apigee--current-policy-name ()
  "returns the name of the current policy, if the current buffer
is visiting a policy file. otherwise nil."
  (let ((current-filename (buffer-file-name)))
    (if (apigee--is-policy-file current-filename)
        (file-name-base current-filename))))
(defun apigee--list-policies (bundle)
  "Return the sorted names of the policies in BUNDLE."
  (sort (mapcar #'file-name-base
                (apigee--proper-files (apigee--bundle-subdir bundle "policies") ".xml"))
        #'string<))

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
  (apigee-setup)
  (let* ((bundle (apigee--require-bundle))
         (current-policies (apigee--list-policies bundle))
         (predicate-ignored nil)
         (require-match nil)
         (initial-input (apigee--current-policy-name))
         (hist nil)
         (def initial-input))
    (let* ((name-of-policy-to-rename
            (completing-read "rename policy: "
                             (apigee--completion-table current-policies)
                             predicate-ignored require-match initial-input hist def))
           (new-name (read-string (format "rename '%s' to : " name-of-policy-to-rename)
                                  name-of-policy-to-rename nil name-of-policy-to-rename)))

      (if (equal name-of-policy-to-rename new-name)
          (message "nothing to do.")
        (if (member new-name current-policies)
            (message "there is already a policy with that name.")
          (let* ((policy-dir (apigee--bundle-subdir bundle "policies")))
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
                                                           (equal interactive-buffer-name (buffer-name)))
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
                         (mapcar (lambda (s) (apigee--bundle-subdir bundle s))
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
  (apigee--with-comment-preserving-xml
   (xml-print elt))
  (apigee--cleanup-newlines)
  (apigee--cleanup-quotes)
  (save-excursion
    (indent-region (point-min) (point-max)))
  )

(defun apigee--verify-exactly-one (file-list source-dir)
  "verifies that there is exactly one xml file."
  (if (not file-list)
      (error "[apigee] cannot find XML file in %s, in `apigee--verify-exactly-one'" source-dir))
  (if (not (= (length file-list) 1))
      (error "[apigee] found more than one XML file in %s, in `apigee--verify-exactly-one'" source-dir))
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
      (when (file-directory-p source-subdir)
        (copy-directory source-subdir dest-subdir t nil)
        (apigee--clean-files-from-dirtree dest-subdir apigee--editor-temp-file-globs)))))

(defun apigee--clean-files-from-dirtree (directory globs)
  "Recursively delete files in DIRECTORY whose names match any of GLOBS."
  (let ((regexp (mapconcat #'wildcard-to-regexp globs "\\|")))
    (dolist (file (directory-files-recursively directory regexp))
      (delete-file file))))

(defun apigee--xml-text-goto-root ()
  "Move point to just after the name of the root element's start tag.
Skips the XML declaration, processing instructions, comments and a
DOCTYPE. Return the element name."
  (goto-char (point-min))
  (let (name)
    (while (not name)
      (unless (search-forward "<" nil t)
        (error "[apigee] no root element in %s" (buffer-name)))
      (cond
       ((looking-at-p "!--") (search-forward "-->"))
       ((looking-at-p "[?!]") (search-forward ">"))
       ((looking-at "[[:alpha:]_][^ \t\n/>]*")
        (goto-char (match-end 0))
        (setq name (match-string-no-properties 0)))))
    name))

(defun apigee--xml-text-set-root-attribute (attr value)
  "Set attribute ATTR of the root element in the current buffer to VALUE.
This edits the text, so the rest of the file keeps its exact formatting.
An existing attribute keeps its quote character; a missing one is added."
  (save-excursion
    (apigee--xml-text-goto-root)
    (let ((tag-end (save-excursion
                     (looking-at "\\(?:[^>\"']\\|\"[^\"]*\"\\|'[^']*'\\)*")
                     (match-end 0)))
          (escaped (xml-escape-string value)))
      (if (re-search-forward (concat "[ \t\n]" (regexp-quote attr)
                                     "[ \t\n]*=[ \t\n]*\\(?:\"\\([^\"]*\\)\"\\|'\\([^']*\\)'\\)")
                             tag-end t)
          (replace-match escaped t t nil (if (match-beginning 1) 1 2))
        (insert " " attr "=\"" escaped "\"")))))

(defun apigee--xml-text-set-element-text (element text)
  "Set the content of the first ELEMENT in the current buffer to TEXT.
Handles both <ELEMENT>...</ELEMENT> and <ELEMENT/>, and keeps the start
tag's attributes. Edits the text, so the rest of the file keeps its
formatting. Return non-nil if there was an ELEMENT to change."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (concat "<" (regexp-quote element) "\\(?:[ \t\n][^>]*?\\)?"
                   "\\(/>\\|>\\(?:.\\|\n\\)*?</" (regexp-quote element) "[ \t\n]*>\\)")
           nil t)
      (replace-match (concat ">" (xml-escape-string text) "</" element ">") t t nil 1)
      t)))

(defun apigee--copy-bundle-template (asset-type asset-name source destination)
  "Copy the ASSET-TYPE template in SOURCE to asset ASSET-NAME in DESTINATION.
The top-level bundle file is renamed to ASSET-NAME.xml, and its name
attribute and DisplayName are set to ASSET-NAME. The kind's :subdirs
are copied, and its :after-copy function, if any, is called. See
`apigee--asset-kinds'."
  (let* ((bundle (apigee--asset-kind asset-type :bundle))
         (source-bundle-dir (apigee--join-path-elements source bundle))
         (dest-bundle-dir (apigee--join-path-elements destination bundle))
         (file-list (apigee--proper-files source-bundle-dir ".xml")))
    (apigee--verify-exactly-one file-list source-bundle-dir)
    ;; create the toplevel destination directory
    (make-directory dest-bundle-dir t)
    ;; copy, then intelligently modify the toplevel bundle definition file
    (let ((new-xml-file-name (apigee--join-path-elements dest-bundle-dir (concat asset-name ".xml"))))
      (copy-file (car file-list) new-xml-file-name nil t t nil)
      (with-current-buffer (find-file-noselect new-xml-file-name)
        (apigee--xml-text-set-root-attribute "name" asset-name)
        (apigee--xml-text-set-element-text "DisplayName" asset-name)
        (save-buffer)))
    (apigee--copy-subdirs (apigee--asset-kind asset-type :subdirs) source-bundle-dir dest-bundle-dir)
    (when-let* ((after-copy (apigee--asset-kind asset-type :after-copy)))
      (funcall after-copy dest-bundle-dir asset-name))))

(defun apigee--set-proxy-basepath (apiproxy-dir proxy-name)
  "Set the BasePath to /PROXY-NAME, if APIPROXY-DIR has a single proxy endpoint."
  (let* ((dest-proxyendpoints-dir (apigee--join-path-elements apiproxy-dir "proxies"))
         (file-list (apigee--proper-files dest-proxyendpoints-dir ".xml")))
    (if (not file-list)
        (error "[apigee] cannot find XML file in %s, in `apigee--set-proxy-basepath'" dest-proxyendpoints-dir))
    (if (= (length file-list) 1)
        (with-current-buffer (find-file-noselect (car file-list))
          (when (apigee--xml-text-set-element-text "BasePath" (concat "/" proxy-name))
            (save-buffer 0))))))

(defun apigee--new-asset-from-template (asset-type asset-name template-alist template-name containing-dir)
  "Non-interactive function to create a new asset from a template.
ASSET-TYPE - either \"proxy\" or \"sharedflow\".
ASSET-NAME - name of the to-be-created asset.
TEMPLATE-ALIST - the alist of templates.
TEMPLATE-NAME - the name of the asset template, as stored in the templates/<asset-type> directory.
CONTAINING-DIR - name of an existing directory into which to insert the new asset.
"
  (if (not (file-directory-p containing-dir))
      (error "[apigee] containing-dir does not exist in `apigee--new-asset-from-template'"))
  (let ((template-match (assoc template-name template-alist))
        (new-dir (apigee--join-path-elements containing-dir asset-name)))
    (if (file-directory-p new-dir)
        (error "[apigee] new-dir already exists in `apigee--new-asset-from-template'"))
    (if (not template-match)
        (error "[apigee] unknown template in `apigee--new-asset-from-template'"))
    (apigee--copy-bundle-template asset-type asset-name (cadr template-match) new-dir)
    (find-file-existing new-dir)))

(defun apigee--fresh-recent-asset-homes (with-time)
  "returns the list of recently used asset homes, eliminating stale entries
and non-existent directories, in sorted order.  If WITH-TIME is non-nil,
emit the list with time (suitable for persisting). Otherwise without -
it will be a bare list of strings representing directory paths, suitable
for use within an `completing-read'."
  (let* ((days-considered-stale 30)
         (fresh (seq-filter (lambda (x)
                              (and
                               (file-directory-p (car x))
                               (apigee--time-is-within-days (encode-time (cadr x)) days-considered-stale)))
                            apigee--recently-used-asset-homes))
         ;; most recent first
         (result (seq-sort (lambda (a b) (time-less-p (encode-time (cadr b)) (encode-time (cadr a))))
                           fresh)))
    (if with-time result
      (mapcar #'car result))))

(defun apigee--time-is-within-days (the-time delta-days)
  "Returns t if THE-TIME is less than DELTA-DAYS ago."
  (let ((delta-seconds (* 24 delta-days 60 60)))
    (< (float-time (time-subtract (current-time) the-time)) delta-seconds)))

(defun apigee--prompt-for-containing-dir ()
  "prompt user for a containing directory, and return it. Create the directory
if necessary."
  (let ((candidate-list (cons default-directory (apigee--fresh-recent-asset-homes nil))))
    (let ((containing-dir
           (completing-read
            "containing directory?: "
            (apigee--completion-table
             (delete-dups (mapcar #'abbreviate-file-name (delq nil candidate-list))))
            nil nil nil)))
      (and (not (file-exists-p containing-dir))
           (make-directory containing-dir t))
      containing-dir)))


(defun apigee-new-proxy (want-prompt)
  "Interactive fn that creates a new exploded proxy bundle directory
structure. Prompts for the name of the API Proxy, and the base template
for the proxy.

When invoked with a prefix, this fn will prompt the user also for
the name of the directory in which to store the apiproxy.

When invoked without a prefix, it uses the most recent asset home
directory. If no directory has ever been used, it prompts for the
directory."
  (interactive "P")
  (apigee--new-asset "proxy" want-prompt))


(defun apigee-new-sharedflow (want-prompt)
  "Interactive fn that creates a new exploded sharedflow bundle directory
structure. Prompts for the name of the sharedflow, and the base template
for the thing.

When invoked with a prefix, this fn will prompt the user also for
the name of the directory in which to store the sharedflow.

When invoked without a prefix, it uses the most recent asset home
directory. If no directory has ever been used, it prompts for the
directory."
  (interactive "P")
  (apigee--new-asset "sharedflow" want-prompt))


(defun apigee--remember-asset-home (containing-dir)
  "stores one directory with the current time in the list"
  (setq apigee--recently-used-asset-homes
        (seq-remove (lambda (x)  (equal (car x) containing-dir)) apigee--recently-used-asset-homes))
  (setq apigee--recently-used-asset-homes (cons (list containing-dir (decode-time nil t))
                                                apigee--recently-used-asset-homes)))

(defun apigee--require-input (value what)
  "Return VALUE, or signal a `user-error' saying WHAT is required."
  (if (apigee--blank-p value)
      (user-error "[apigee] %s is required" what)
    value))

(defun apigee--new-asset (asset-type want-prompt)
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
WANT-PROMPT - whether invoked with a prefix

"
  (apigee-setup)
  (let ((alist-sym (apigee--asset-kind asset-type :templates)))
    (let ((asset-name
           (apigee--require-input
            (read-string (format "%s name?: " asset-type) nil nil nil)
            (format "A %s name" asset-type)))
          (template
           (apigee--require-input
            (completing-read
             (format "%s template: " asset-type)
             (apigee--completion-table
              (mapcar (lambda (x) (car x)) (symbol-value alist-sym)))
             nil t nil)
            (format "A %s template" asset-type)))
          (containing-dir
           (if want-prompt
               (apigee--prompt-for-containing-dir)
             (or (car (apigee--fresh-recent-asset-homes nil)) (apigee--prompt-for-containing-dir)))))
      (apigee--remember-asset-home containing-dir)
      (apigee--new-asset-from-template asset-type asset-name (symbol-value alist-sym) template containing-dir))))


;; 20250527-1829 - no longer using popup.
;; ;; override the impl in popup.el - it's broken for me for cascaded menus - 20230627-1541
;; (defun popup-child-point (popup &optional offset)
;;   (overlay-end
;;    (popup-line-overlay popup (popup-selected-line popup))))

;;      ;; customize the popup font
;; (set-face-attribute 'popup-menu-face nil :height 105)
;; (set-face-attribute 'popup-menu-selection-face nil :height 105)

(defvar apigee--setup-done nil
  "Non-nil once `apigee-setup' has run.")

(defun apigee-setup ()
  "Set up apigee.el. Loading the file has no side effects; this does them.
Restore the saved settings, load the templates, teach compilation mode
the apigeelint output format, and save the settings every
`apigee-timer-minutes' and when Emacs exits.

Every apigee command calls this first, so there's no need to call it
yourself, though you can, e.g. to load the templates early. Calling it
again does nothing. `unload-feature' undoes it."
  (interactive)
  (unless apigee--setup-done
    (apigee--register-compilation-error-regexps)
    ;; restore last known state, and set a timer to persist state periodically
    (apigee--restore-state)
    (if (not apigee--base-template-dir)
        (setq apigee--base-template-dir (concat (file-name-directory apigee--load-file-name) "templates/")))
    (if apigee--base-template-dir
        (apigee-load-templates apigee--base-template-dir))
    (when (timerp apigee--timer)
      (cancel-timer apigee--timer))
    (setq apigee--timer
          (run-with-timer (* 60 apigee-timer-minutes) (* 60 apigee-timer-minutes) 'apigee--persist-state))
    ;; Also save on exit, so changes since the last timer run are kept.
    (add-hook 'kill-emacs-hook #'apigee--persist-state)
    (setq apigee--setup-done t)))

(defun apigee-unload-function ()
  "Undo `apigee-setup', when `unload-feature' unloads apigee.
Saves the settings first. Returns nil, so normal unloading continues."
  (when apigee--setup-done
    (apigee--persist-state))
  (when (timerp apigee--timer)
    (cancel-timer apigee--timer))
  (remove-hook 'kill-emacs-hook #'apigee--persist-state)
  (setq compilation-error-regexp-alist
        (seq-difference compilation-error-regexp-alist apigee--compilation-error-symbols))
  (setq compilation-error-regexp-alist-alist
        (seq-remove (lambda (entry) (memq (car entry) apigee--compilation-error-symbols))
                    compilation-error-regexp-alist-alist))
  nil)

(provide 'apigee)

;;; apigee.el ends here
