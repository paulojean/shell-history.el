;;; shutils-history-helm.el --- access history from shutils-mode

;;; requires

;;; Commentary:

;;; Code:

(require 'helm)
(require 'shutils-history)

(defun shutils-history-helm/build-source (history)
  (helm-build-sync-source "Shell history: "
    :fuzzy-match t
    :candidates history
    :action (lambda (cmd)
              (progn
                (shutils-history/delete-current-line)
                (insert cmd)))))

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
