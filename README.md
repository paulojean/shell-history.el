# shtutils.el

Utility functions to improve emacs shell.

## Instalation

### Dependencies
- [`dash`](https://github.com/magnars/dash.el)

Either
- [`helm`](https://github.com/emacs-helm/helm)
- [`ivy`](https://github.com/abo-abo/swiper)

### Initialising

Add all `.el` files to a directory and load it from emacs. Eg: you can copy everything to `~/.emacs.d/lisp/`, then load this path in your `init.el`

``` el
(add-to-list 'load-path "~/.emacs.d/lisp")
;;; if you use `helm`
(require 'shutils-history-helm)
;;; if you use `ivy`
(require 'shutils-history-ivy)

;;; The following line is optional, but recommened.
;;; It makes possible to show new commands (sent from `emacs` shell) in the history's list.
;;; For dofumentation, see: https://github.com/paulojean/shutils.el/blob/master/shutils-history.el
(shutils-history/start-auto-update)

```

## Using it

Now you can define a key sequence to load the history fuzzy finder, eg: assuming mode, you can do the following:

```el
;;; for `helm`
(progn
  (evil-define-key 'normal shell-mode-map
    (kbd "C-r") 'shell-history-helm/show-history)
  (evil-define-key 'insert shell-mode-map
    (kbd "C-r") 'shutils-history-helm/show-history))

;;; for `ivy`
(progn
  (evil-define-key 'normal shell-mode-map
    (kbd "C-r") 'shell-history-ivy/show-history)
  (evil-define-key 'insert shell-mode-map
    (kbd "C-r") 'shutils-history-ivy/show-history))
```

And you can use `C-r` (in a shell buffer) to load the shell history (with fuzzy finder), pick a command and put it in the shell.

Alternativelly, you can just `M-x shutils-history-helm/show-history` from a shell buffer.
