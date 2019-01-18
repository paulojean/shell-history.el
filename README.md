# shtutils.el

Utility functions to improve emacs shell.

## Instalation

You can use [straight.el](https://github.com/raxod502/straight.el#but-what-about-my-fork-of-obscure-el-package) to fetch the package.

### Dependencies
`shtutils.el` uses [`helm`](https://github.com/emacs-helm/helm) and [`dash`](https://github.com/magnars/dash.el), so it assumes that you've installed them as well.

### Initialising

``` el
(require 'helm)
(require 'dash)
(require 'shutils.el)

(shell-history/start-auto-update) ;; https://github.com/paulojean/shutils.el/blob/master/shell-history.el for documentation

```

Now you can define a key sequence to load the history fuzzy finder, eg: assuming mode, you can do the following:

```el
(progn
  '(evil-set-initial-state 'shell-mode 'normal)
  (evil-define-key 'normal shell-mode-map
    (kbd "C-r") 'shell-history/show-history)
```

