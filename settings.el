;; settings.el: general emacs configuration

;; frame
(setq default-frame-alist
    '((height . 35)
      (width . 95)))
(add-hook 'server-after-make-frame-hook
	(lambda ()
	    (select-frame-set-input-focus (selected-frame))
	    (set-frame-parameter nil 'width 95)
	    (set-frame-parameter nil 'height 35)))

;; setup
(setq native-comp-async-report-warnings-errors nil
      backup-directory-alist '(("." . "~/.saves"))
      indent-tabs-mode t
      tab-width 4)

;; modes
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(xterm-mouse-mode 1)
(line-number-mode -1)
(global-visual-line-mode 1)

;; keybinds
(global-unset-key (kbd "M-s"))
(global-unset-key (kbd "K"))
(evil-global-set-key 'normal (kbd "H") #'windmove-left)
(evil-global-set-key 'normal (kbd "L") #'windmove-right)
(evil-global-set-key 'normal (kbd "K") #'windmove-up)
(evil-global-set-key 'normal (kbd "J") #'windmove-down)
(evil-global-set-key 'normal (kbd "U") #'ao/kill-window-and-maybe-buffer)
;; See how to make the following two keybinds good
;; (evil-global-set-key 'normal (kbd "*") #'kill-current-buffer)
;; (evil-global-set-key 'normal (kbd "(") #'delete-window)
(global-set-key (kbd "M-s") #'switch-to-buffer)
(global-set-key (kbd "M-x") #'ao/toggle-execute-extended-command)

;; hooks
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(setq org-startup-indented t)

;; ido
(ido-mode 1)
