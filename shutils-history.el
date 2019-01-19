;;; shutils-history.el --- access history from shutils-mode

;;; requires

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'dash)
(require 'comint)

;;; Code:

(defvar shutils-history/cache '())

(defun shutils-history/parse-history (history)
  "Gets a list of commands in raw formart and remove leading white spaces and history number"
  (-map (lambda (h)
          (replace-regexp-in-string "[[:space:]]*[0-9][[:space:]]*"
                                    ""
                                    h))
        history))

(defun shutils-history/get-history! (&optional refresh)
  "Executes a shell command to get the history"
  (if (and (not refresh)
           (not (eq nil shutils-history/cache)))
    shutils-history/cache
    (let ((history (-> "HISTFILE=~/.bash_history && set -o history && history | grep -Ev \"[^ -~]\""
                       shell-command-to-string
                       (split-string "\n")
                       shutils-history/parse-history
                       reverse)))
      (setq shutils-history/cache history))))

(defun shutils-history/clear-cache! ()
  (setq shutils-history/cache '()))

;;; extracted from https://stackoverflow.com/a/35711240/3939522
(defun shutils-history/delete-current-line ()
  "Delete (not kill) the current line."
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(defun shutils-history/build-helm-source (history)
  (helm-build-sync-source "Shell history: "
    :fuzzy-match t
    :candidates history
    :action (lambda (cmd)
              (progn
                (shutils-history/delete-current-line)
                (insert cmd)))))

;;; modified from https://github.com/emacs-mirror/emacs/blob/be505726b68d407a44fdcd9c7ac1ef722398532d/lisp/comint.el#L1772
(defun shutils-history/read-current-input ()
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (process-mark proc)))
    (goto-char (field-end))
    (buffer-substring-no-properties pmark (point))))

(defun shutils-history/insert-current-line-to-cache! (&optional no-newline artificial)
  (if (not no-newline) ;;; don't store if the command was aborted, ie: "C-c C-c"
      (setq shutils-history/cache (cons (shutils-history/read-current-input)
                                      (shutils-history/get-history!)))))

;;;###autoload
(defun shutils-history/show-history ()
  "Open shell history and insert the selected command in the buffer."
  (interactive)
  (let* ((src (-> (shutils-history/get-history!)
                  shutils-history/build-helm-source))
         (current-input (shutils-history/read-current-input)))
    (helm :sources src
          :input current-input)))

;;;###autoload
(defun shutils-history/stop-auto-update ()
  (interactive)
  "Stop caching commands as they are sent to `shell`.
May result in recent commands not being displayed when invoking `shutils-history/show-history`"
  (advice-remove 'comint-send-input 'shutils-history/insert-current-line-to-cache!))

;;;###autoload
(defun shutils-history/start-auto-update ()
  (interactive)
  "Caches every new command that is sent to `shell`."
    (advice-add 'comint-send-input :before 'shutils-history/insert-current-line-to-cache! ))

(provide 'shutils-history)

;;; shutils-history.el ends here
