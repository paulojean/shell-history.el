;;; shutils-history.el --- access shell history from emacs' shell-mode

;; Author: Paulo Sousa <pauloj10@gmail.com>
;; Package-Requires: ((dash "2.16.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'comint)
(require 'subr-x)

(defvar shutils-history/cache '())

(defvar shutils-history/bash-history-path "~/.bash_history")

(setq *shutils-history/partial-command* "")

(defun shutils-history/parse-history (history)
  "Gets a list of commands in raw formart and remove leading white spaces and history number"
  (--map (replace-regexp-in-string "[[:space:]]*[0-9][[:space:]]*"
                                   ""
                                   it)
         history))

(defun shutils-history/get-history! (&optional refresh)
  "Executes a shell command to get the history"
  (if (and (not refresh)
           (not (eq nil shutils-history/cache)))
      shutils-history/cache
    (progn
      (message "shutils: fetching history...")
      (let ((history (with-temp-buffer
                       (insert-file-contents shutils-history/bash-history-path)
                       (->> (split-string (buffer-string) "\n")
                            (--filter (string-match "^[ -~]+$" it))
                            (--filter (<= 5 (length it)))
                            reverse))))
        (setq shutils-history/cache history) ))))

(defun shutils-history/clear-cache! ()
  (setq shutils-history/cache '()))

;;; extracted from https://stackoverflow.com/a/35711240/3939522
(defun shutils-history/delete-current-line ()
  "Delete (not kill) the current line."
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

;;; modified from https://github.com/emacs-mirror/emacs/blob/be505726b68d407a44fdcd9c7ac1ef722398532d/lisp/comint.el#L1772
(defun shutils-history/read-current-input ()
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (process-mark proc)))
    (goto-char (field-end))
    (buffer-substring-no-properties pmark (point))))

(defun shutils-history/is-trivial-command? (command)
  (-> command
      length
      (<= 6)))

(defun shutils-history/is-repeated-command? (command history)
  (-> history
      first
      (equal command)))

(defun shutils-history/prefix-with-partial-commands (command)
  (-> *shutils-history/partial-command*
      (concat " " (string-remove-suffix "\\" command))
      string-trim
      (->> (replace-regexp-in-string "\\\\\n" "")
           (replace-regexp-in-string "\n" ""))))

(defun shutils-history/should-add-command-to-cache? (new-command history)
  (not (or (shutils-history/is-repeated-command? new-command history)
           (shutils-history/is-trivial-command? new-command)) ))

(defun shutils-history/insert-current-line-to-cache! (&optional no-newline artificial)
  (if no-newline ;;; don't save if the command was aborted, ie: "C-c C-c"
      (setq *shutils-history/partial-command* "")
    (let* ((new-command (string-trim (shutils-history/read-current-input)))
           (history (shutils-history/get-history!))
           (complete-command (shutils-history/prefix-with-partial-commands (string-remove-suffix "\\" new-command)))
           (is-partial-command? (string-suffix-p "\\" new-command)))
      (if is-partial-command?
          (setq *shutils-history/partial-command* complete-command)
        (progn
          (when (shutils-history/should-add-command-to-cache? complete-command history)
            (setq shutils-history/cache (cons complete-command history)))
          (setq *shutils-history/partial-command* ""))))))

(defun shutils-history/replace-input-with-command (command)
  "Clear shell's current input and add COMMAND to it."
  (shutils-history/delete-current-line)
  (insert command))

;;;###autoload
(defun shutils-history/stop-auto-update ()
  (interactive)
  "Stop caching commands as they are sent to `shell`.
May result in recent commands not being displayed when invoking `shutils-history/show-history`"
  (advice-remove 'comint-send-input 'shutils-history/insert-current-line-to-cache!))

;;;###autoload
(defun shutils-history/start-auto-update ()
  (interactive)
  "Caches every new command that is sent to `shell`.

Without this you will have access only to the previous already in `shutils-history/cache`
(by default, it will be whatever you have in ~/.bash_history` when first calling `shutils-history/show-history`."
  (advice-add 'comint-send-input :before 'shutils-history/insert-current-line-to-cache! ))

(provide 'shutils-history)

;;; shutils-history.el ends here
