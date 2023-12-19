# apigee-el

Emacs elisp module for interacting with Apigee, and for creating and modifying Apigee assets.

## Using it

Apigee Edge is an API platform that allows you to configure smart HTTP proxies for your APIs.  Within the proxies you can configure Apigee to do things like :
 - verify an inbound API key
 - verify an inbound OAuth 2.0 token
 - rewrite the URL
 - route the HTTP request
 - inject or remove headers , query params
 - modify the payload
 - modify the verb
 - and many other things.

This module is an emacs-lisp module that adds capabilities to emacs to make it easier to create and edit API proxies offline, within the editor. Apigee includes a User Interface with an interactive, graphical proxy designer. For people who like to work offline, this emacs-lisp module is somewhat similar.

This module works to generate proxies suitable for use in Apigee Edge or in X/hybrid.

To use this module, put this in your .emacs file:

```
 (add-to-list 'load-path "~/elisp/apigee")
 (require 'apigee)
```

Then, you can invoke various commands:

To add a new proxy:

    M-x apigee-new-proxy

To specify the directory in which to store the new proxy:

    C-u M-x apigee-new-proxy

To add a policy to an existing proxy, open a dired buffer to ../apiproxy and:

    M-x apigee-add-policy

To add a target to an existing proxy, open a dired buffer to ../apiproxy and:

    M-x apigee-add-target

To run apigeelint on the proxy found in the current working directory:

    M-x apigee-lint-proxy

To import the proxy found in the current working directory (works only with X/hybrid):

    M-x apigee-import-proxy

To deploy the proxy found in the current working directory (works only with X/hybrid):

    M-x apigee-deploy-proxy


## Adding templates

There are some templates for proxies and policies included in the module.
If you wish to add to that set of templates, just drop more files into the approprpiate directories. Find them at <apigee-el>/templates/proxies and
<apigee-el>/templates/policies .

## Customizing the External Programs

The module uses external programs to import, deploy, or run apigeelint.  To use
them you need to have previously installed apigeelint and apigeecli. You may
want to set specific settings for the external programs. To do so, put something
like this in your emacs init file:

```lisp
(eval-after-load "apigee"
  '(progn
     (setcar (alist-get 'apigeecli apigee-programs-alist)
         "~/.apigeecli/bin/apigeecli")
     (setcar (alist-get 'apigeelint apigee-programs-alist)
         "node ~/dev/apigeelint/cli.js")
     (setq apigee-environment (getenv "ENV")
           apigee-organization (getenv "ORG"))
     (setq apigee-gcloud-program "~/google-cloud-sdk/bin/gcloud")
     ))
```


## Enhancements

Pull requests are welcome.

Some ideas for extending this module:

- listing/querying of Proxies, Products, Developers and Apps
- read/update of KVM entries
- support for shared flows


## License

This material is Copyright 2017-2023 Dino Chiesa and Google LLC.
and is licensed under the [Apache 2.0 License](LICENSE). This includes the elisp code as well as any API Proxy templates.


## Disclaimer

This module is not an official Google product, nor is it part of an official Google product.
