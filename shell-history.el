;;; shell-history.el --- access history from shell-mode

;;; requires

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'dash)
(require 'comint)

;;; Code:

(defvar shell-history/cache '())

(defun shell-history/parse-history (history)
  "Gets a list of commands in raw formart and remove leading white spaces and history number"
  (-map (lambda (h)
          (replace-regexp-in-string "[[:space:]]*[0-9][[:space:]]*"
                                    ""
                                    h))
        history))

(defun shell-history/get-history! (&optional refresh)
  "Executes a shell command to get the history"
  (if (and (not refresh)
           (not (eq nil shell-history/cache)))
    shell-history/cache
    (let ((history (-> "HISTFILE=~/.bash_history && set -o history && history | grep -Ev \"[^ -~]\""
                       shell-command-to-string
                       (split-string "\n")
                       shell-history/parse-history
                       reverse)))
      (setq shell-history/cache history))))

(defun shell-history/clear-cache! ()
  (setq shell-history/cache '()))

;;; extracted from https://stackoverflow.com/a/35711240/3939522
(defun shell-history/delete-current-line ()
  "Delete (not kill) the current line."
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(defun shell-history/build-helm-source (history)
  (helm-build-sync-source "Shell history: "
    :fuzzy-match t
    :candidates history
    :action (lambda (cmd)
              (progn
                (shell-history/delete-current-line)
                (insert cmd)))))

;;; modified from https://github.com/emacs-mirror/emacs/blob/be505726b68d407a44fdcd9c7ac1ef722398532d/lisp/comint.el#L1772
(defun shell-history/read-current-input ()
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (process-mark proc)))
    (goto-char (field-end))
    (buffer-substring-no-properties pmark (point))))

(defun shell-history/insert-current-line-to-cache! (&optional no-newline artificial)
  (interactive)
  (if (not no-newline) ;;; don't store if the command was aborted, ie: "C-c C-c"
      (setq shell-history/cache (cons (shell-history/read-current-input)
                                      (shell-history/get-history!)))))

(defun shell-history/show-history ()
  "Open shell history and insert the selected command in the buffer."
  (interactive)
  (let* ((src (-> (shell-history/get-history!)
                  shell-history/build-helm-source))
         (current-input (shell-history/read-current-input)))
    (helm :sources src
          :input current-input)))

(defun shell-history/stop-auto-update ()
  "Stop caching commands as they are sent to `shell`.
May result in recent commands not being displayed when invoking `shell-history/show-history`"
  (advice-remove 'comint-send-input 'shell-history/insert-current-line-to-cache!))

(defun shell-history/start-auto-update ()
  "Caches every new command that is sent to `shell`."
    (advice-add 'comint-send-input :before 'shell-history/insert-current-line-to-cache! ))

(provide 'shell-history)

;;; shell-history.el ends here
