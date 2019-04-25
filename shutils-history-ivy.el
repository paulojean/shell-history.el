;;; shutils-history-ivy.el --- access history using ivy as completion frontend

;; Author: Paulo Sousa <pauloj10@gmail.com>
;; Package-Requires: ((ivy "0.11.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'ivy)
(require 'shutils-history)

;;;###autoload
(defun shutils-history-ivy/show-history ()
  "Open shell history and insert the selected command in the buffer."
  (interactive)
  (when-let ((history (shutils-history/get-history!))
             (current-input (shutils-history/read-current-input))
             (cmd (ivy-read "Shell history: " history :initial-input current-input)))
    (shutils-history/delete-current-line)
    (insert cmd)))

(provide 'shutils-history-ivy)

;;; shutils-history-ivy.el ends here
