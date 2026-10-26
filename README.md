# apigee-el

Emacs elisp module for interacting with Apigee, and for creating and modifying Apigee assets.

## Using it

Apigee is an API platform that allows you to configure smart HTTP proxies for your APIs.  Within the proxies you can configure Apigee to do things like :
 - verify an inbound API key
 - verify an inbound OAuth 2.0 token
 - rewrite the URL
 - route the HTTP request
 - inject or remove headers , query params
 - modify the payload
 - modify the verb
 - and many other things.

This module is an emacs-lisp module that adds capabilities to emacs to make it easier to create and edit API proxies and sharedflows offline, within the editor. Apigee includes a User Interface with an interactive, graphical proxy designer. For people who like to work offline, this emacs-lisp module is somewhat similar.

This module works to generate proxies and sharedflows suitable for use in Apigee Edge or in X/hybrid.

### Requirements

- Emacs 28.1 or later
- [yasnippet](https://github.com/joaotavora/yasnippet); the policy and target templates are snippets
- For the lint, validate, import and deploy commands: [apigeelint](https://github.com/apigee/apigeelint), the Apigee policy schema validator, [apigeecli](https://github.com/apigee/apigeecli) and gcloud. See [Customizing the external programs](#customizing-the-external-programs).

### Installing

To use this module, put this in your .emacs file:

```lisp
(add-to-list 'load-path "~/elisp/apigee")
(require 'apigee)
```

Or, with `use-package`, deferring the load until the first command:

```lisp
(use-package apigee
  :if (file-exists-p "~/elisp/apigee/apigee.el")
  :load-path "~/elisp/apigee"
  :defer t
  :commands (apigee-new-proxy apigee-new-sharedflow apigee-add-policy
             apigee-lint-asset apigee-import-and-deploy-proxy))
```

Loading the module has no side effects. The first apigee command calls
`apigee-setup`, which restores the saved settings, loads the templates, and
teaches compilation mode the apigeelint output format. You can call
`M-x apigee-setup` yourself, for example to load the templates early.
`unload-feature` undoes it.

### Commands

To add a new proxy or sharedflow:

    M-x apigee-new-proxy
    M-x apigee-new-sharedflow

To specify the directory in which to store the new proxy or sharedflow:

    C-u M-x apigee-new-proxy
    C-u M-x apigee-new-sharedflow

The remaining commands work on the bundle that contains the current
directory, so run them from anywhere inside it, for example a dired buffer
on ../apiproxy or ../sharedflowbundle.

To add a policy to an existing proxy or sharedflow:

    M-x apigee-add-policy

To add a target to an existing proxy:

    M-x apigee-add-target

To rename a policy, and update the references to it:

    M-x apigee-rename-policy

To add policies and steps that return the proxy revision in a response header
(running it again changes nothing):

    M-x apigee-inject-proxy-revision-logic

To run apigeelint, or the XML schema validator, on the current proxy or sharedflow:

    M-x apigee-lint-asset
    M-x apigee-validate-xml-for-asset

To import, deploy, or import and then deploy, the current proxy or sharedflow
(works only with X/hybrid):

    M-x apigee-import-proxy
    M-x apigee-deploy-proxy
    M-x apigee-import-and-deploy-proxy

With `C-u`, the lint, validate, import and deploy commands let you edit the
command line before it runs. The access token shows as `%t` while you edit,
and is filled in afterwards. The import and deploy commands prompt for the
organization and environment when they aren't set.

To load templates from a different directory:

    M-x apigee-load-templates


## Adding templates

There are some templates for proxies, sharedflows, policies and targets
included in the module. If you wish to add to that set of templates, just drop
more files into the appropriate directories. Find them at

- `<apigee-el>/templates/proxies` and `<apigee-el>/templates/sharedflows`: one
  directory per template, holding an exploded `apiproxy` or `sharedflowbundle`
- `<apigee-el>/templates/policies/<PolicyType>`: one XML file per template
- `<apigee-el>/templates/targets`: one XML file per template

Policy and target templates are yasnippet snippets, so they can contain fields
like `${1:default}`. Each `##` in a template is replaced with the name of the
new policy or target.

To keep your templates elsewhere, use `M-x apigee-load-templates`; the
directory you choose is remembered.

## Customizing the external programs

The module uses external programs to import, deploy, lint and validate. To use
them you need to have previously installed apigeelint and apigeecli, and gcloud,
which supplies the access token.

The settings are in a customization group:

    M-x customize-group RET apigee

Or put something like this in your emacs init file:

```lisp
(with-eval-after-load "apigee"
  (setf (alist-get 'apigeecli apigee-programs-alist)
        "~/.apigeecli/bin/apigeecli"
        (alist-get 'apigeelint apigee-programs-alist)
        "node ~/dev/apigeelint/cli.js"
        (alist-get 'gcloud apigee-programs-alist)
        "~/google-cloud-sdk/bin/gcloud")
  (setq apigee-environment (getenv "ENV")
        apigee-organization (getenv "ORG")
        apigee-xmlschema-validator-home "~/dev/apigee-schema-inference/dist"))
```

A program is a name (looked up on the `PATH`), a path (spaces are fine), a
program followed by arguments, or a list of program and arguments.

The settings you're most likely to want:

| Setting | Meaning |
|---|---|
| `apigee-programs-alist` | The programs: `gcloud`, `apigeecli`, `apigeelint`, `validate-cmd` |
| `apigee-organization`, `apigee-environment` | Where to import and deploy; prompted for when unset |
| `apigee-service-account` | The deployment service account; `-` means none |
| `apigee-lint-exclusions` | apigeelint plugins to skip |
| `apigee-xmlschema-validator-home` | The directory holding the schema validator and its Python venv |
| `apigee-commands-alist` | The command lines that the commands run |

Each command in `apigee-commands-alist` is a list of words. A word `%KEY`
expands through `apigee-programs-alist` or `apigee-placeholders-alist`: for
example `%n` is the proxy or sharedflow name, `%o` the organization, `%t` the
access token. Every word is shell-quoted. A command can also be a list of other
command names, run in sequence with `&&`, or a plain shell command string.

## Saved settings

The organization, environment, service account, template directory and recently
used directories are saved in `apigee-el.dat` in `user-emacs-directory`, every
`apigee-timer-minutes` minutes and when Emacs exits. A setting that already has
a value when the module sets up, for example from your init file, is not
overwritten.

## Tests

The tests are in `apigee-tests.el`. Run them from this directory with:

```sh
emacs -Q --batch -L . -f package-initialize \
  -l apigee-tests.el -f ert-run-tests-batch-and-exit
```

They use a temporary `user-emacs-directory`, so they don't touch your saved
settings. They also check that apigee.el byte-compiles without warnings and that
checkdoc finds nothing to report.

## Enhancements

Pull requests are welcome.

Some ideas for extending this module:

- listing/querying of Proxies, Products, Developers and Apps
- read/update of KVM entries


## License

This material is Copyright 2017-2026 Dino Chiesa and Google LLC.
and is licensed under the [Apache 2.0 License](LICENSE). This includes the elisp code as well as any API Proxy templates.


## Disclaimer

This module is not an official Google product, nor is it part of an official Google product.
