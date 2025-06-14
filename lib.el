;; lib.el: defines miscellaneous helpers used throughout the config

(defvar rc/package-contents-refreshed nil)

(defun rc/refresh-packages-once ()
  (when (not rc/package-contents-refreshed)
    (setq rc/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc/require-one-package (package)
  (when (not (package-installed-p package))
    (rc/refresh-packages-once)
    (package-install package)))

(defun rc/require (&rest packages)
  (dolist (package packages)
    (rc/require-one-package package)))

(defun ao/toggle-execute-extended-command ()
  (interactive)
  (if (minibufferp)
      (keyboard-escape-quit)
    (call-interactively 'execute-extended-command)))

(defun ao/kill-window-and-maybe-buffer ()
  "Kill the current window. Kill its buffer if it's not displayed in any other window."
  (interactive)
  (let* ((buf  (current-buffer))
	 (wins (get-buffer-window-list buf nil t)))
    (unless (delq (selected-window) wins)
      (kill-buffer buf))
    (delete-window)))

(autoload 'delete-selection-helper "delsel")
(defun vz/delete-mouse-selection-pre-hook ()
  (when (and vz/delete-mouse-selection-mode
             (use-region-p) (mouse-region-match)
             (not buffer-read-only))
    (delete-selection-helper (and (symbolp this-command)
                                  (get this-command 'delete-selection)))))

(define-minor-mode vz/delete-mouse-selection-mode
  "Replace the region by typed text if it was selected using the mouse.
Otherwise, typed text is just inserted."
  :global t
  (if vz/delete-mouse-selection-mode
      (add-hook 'pre-command-hook #'vz/delete-mouse-selection-pre-hook)
    (remove-hook 'pre-command-hook #'vz/delete-mouse-selection-pre-hook)))
