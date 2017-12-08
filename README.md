# apigee-edge-el

Emacs elisp module for interacting with Apigee Edge, and for creating and modifying Apigee Edge assets.

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

This module is an emacs-lisp module that adds capabilities to emacs to make it easier to create and edit API proxies within the editor. Apigee Edge includes a User Interface with an interactive, graphical proxy designer. For people who like to work offline, this emacs-lisp module is somewhat similar.

To use this module, put this in your .emacs file:

```
 (add-to-list 'load-path "~/elisp/apigee")
 (require 'apigee-edge)
```

Then, you can invoke various commands:

To add a new proxy:

    M-x edge-new-proxy

To specify the directory in which to store the new proxy:

    C-u M-x edge-new-proxy

To add a policy to an existing proxy, open a dired buffer to ../apiproxy and:

    M-x edge-add-policy

To add a target to an existing proxy, open a dired buffer to ../apiproxy and:

    M-x edge-add-target


## Adding templates

There are some templates for proxies and policies included in the module.
If you wish to add to that set of templates, just drop more files into the approprpiate directories. Find them at <apigee-edge-el>/templates/proxies and
<apigee-edge-el>/templates/policies .


## Extensions

Pull requests are welcome.

Some ideas for extending this module:

- import and deploy of API proxy bundles from emacs
- listing/querying of Proxies, Products, Developers and Apps
- read/update of KVM entries
- support for shared flows



## License

This material is Copyright 2017 Dino Chiesa and Google Inc.
and is licensed under the [Apache 2.0 License](LICENSE). This includes the elisp code as well as any API Proxy templates.


## Disclaimer

This module is not an official Google product, nor is it part of an official Google product.

