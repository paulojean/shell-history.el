# shtutils.el

Utility functions to improve emacs shell.

## Instalation

### Dependencies
- [`helm`](https://github.com/emacs-helm/helm)
- [`dash`](https://github.com/magnars/dash.el)

### Initialising

Add all `.el` files to a directory and load it from emacs. Eg: you can copy everything to `~/.emacs.d/lisp/`, then load this path in your `init.el`

``` el
(add-to-list 'load-path "~/.emacs.d/lisp")
;;; if you use [`helm`](https://github.com/emacs-helm/helm)
(require 'shutils-history-helm)

;;; The following line is optional, but recommened.
;;; It makes possible to show new commands (sent from `emacs` shell) in the history's list.
;;; For dofumentation, see: https://github.com/paulojean/shutils.el/blob/master/shutils-history.el
(shutils-history/start-auto-update)

```

## Using it

Now you can define a key sequence to load the history fuzzy finder, eg: assuming mode, you can do the following:

```el
(progn
  (evil-define-key 'normal shell-mode-map
    (kbd "C-r") 'shell-history-helm/show-history)
  (evil-define-key 'insert shell-mode-map
    (kbd "C-r") 'shutils-history-helm/show-history))
```

And you can use `C-r` (in a shell buffer) to load the shell history (with fuzzy finder), pick a command and put it in the shell.

Alternativelly, you can just `M-x shutils-history-helm/show-history` from a shell buffer.
