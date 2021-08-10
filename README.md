![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)
![MELPA](https://melpa.org/packages/flymake-proselint-badge.svg)

# flymake-proselint

This package makes it possible to use [proselint](http://proselint.com/) with Emacs built-in Flymake.

## Getting started

`flymake-proselint` is listed on MELPA. If you use `use-package` you could install
it with something like:

``` emacs-lisp
(use-package flymake-proselint
  :ensure t)
```

Or you can use [`straight.el`](https://github.com/raxod502/straight.el) to install it from this repository instead:

``` emacs-lisp
(straight-use-package
  '(flymake-proselint :host github :repo "manuel-uberti/flymake-proselint"))
```

Then you just need to activate it in the modes you want your prose to be
checked:

``` emacs-lisp
(add-hook 'text-mode-hook (lambda ()
                            (flymake-mode +1)
                            (flymake-proselint-setup)))
```


