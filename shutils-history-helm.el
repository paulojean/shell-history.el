;;; shutils-history-ivy.el --- access history using helm as completion frontend

;; Author: Paulo Sousa <pauloj10@gmail.com>
;; Package-Requires: ((helm "3.2"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'helm)
(require 'shutils-history)

(defun shutils-history-helm/build-source (history)
  "Create build source, with fuzzy finder, to HISTORY."
  (helm-build-sync-source "Shell history: "
    :fuzzy-match t
    :candidates history
    :action 'shutils-history/replace-input-with-command))

;;;###autoload
(defun shutils-history-helm/show-history ()
  "Open shell history and insert the selected command in the buffer."
  (interactive)
  (let* ((src (-> (shutils-history/get-history!)
                  shutils-history-helm/build-source))
         (current-input (shutils-history/read-current-input)))
    (helm :sources src
          :input current-input)))

(provide 'shutils-history-helm)

;;; shutils-history-helm.el ends here
