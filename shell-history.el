;;; shell-history.el --- access history from shell-mode

;;; requires

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'helm)

;;; Code:

(defun shell-history-get-history ()
  "Executes a shell command to get the history"
  (let* ((history-command "HISTFILE=~/.bash_history && set -o history && history")
         (split-string (shell-command-to-string history-command)
                       "\n"))))

(defun shell-history-parse-history (history)
  "Gets a list of commands in raw formart and remove leading white spaces and history number"
  (mapcar (lambda (h)
            (replace-regexp-in-string "[[:space:]]*[0-9][[:space:]]*"
                                      ""
                                      h))
          history))

(defun shell-history ()
  "Open shell history and insert the selected command in the buffer"
  (interactive)
  (let* ((raw-history (shell-history-get-history))
         (history: (shell-history-parse-history raw-history)))
    (helm :prompt "File: "
          :sources  `(((name       . "Shell history: ")
                       (candidates . ,history)
                       (action     . (lambda (cmd) (insert cmd))))))))

(provide 'shell-history)

;;; shell-history.el ends here
