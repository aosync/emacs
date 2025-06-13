;; lib.el: defines miscellaneous helpers used throughout the config

(defun ao/toggle-execute-extended-command ()
  (interactive)
  (if (minibufferp)
      (keyboard-escape-quit)
    (call-interactively 'execute-extended-command)))

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
