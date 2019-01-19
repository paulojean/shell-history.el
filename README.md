# shtutils.el

Utility functions to improve emacs shell.

## Instalation

### Dependencies
`shtutils.el` uses [`helm`](https://github.com/emacs-helm/helm) and [`dash`](https://github.com/magnars/dash.el), so it assumes that you've installed them as well.

### Initialising

Add all `.el` files to a directory and load it from emacs. Eg: you can copy everything to `~/.emacs.d/lisp/`, then load this path in your `init.el`

``` el
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'shutils.el)

;;; The following line is optional, but recommened.
;;; For dofumentation, see: https://github.com/paulojean/shutils.el/blob/master/shutils-history.el
;;; But it makes sure that commands sent from `emacs` shell are shown in the list suggestions.
(shutils-history/start-auto-update)

```

## Using it

Now you can define a key sequence to load the history fuzzy finder, eg: assuming mode, you can do the following:

```el
(progn
  '(evil-set-initial-state 'shell-mode 'normal)
  (evil-define-key 'normal shell-mode-map
    (kbd "C-r") 'shell-history/show-history)
  (evil-define-key 'insert shell-mode-map
    (kbd "C-r") 'shutils-history/show-history))
```

And you can use `C-r` (in a shell buffer) to load the shell history (with fuzzy finder) and pick a command to put in the shell.

Alternativelly, you can just `M-x shutils-history/show-history` from a shell buffer.
