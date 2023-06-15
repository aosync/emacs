;; settings.el: general emacs configuration

;; setup
(setq native-comp-async-report-warnings-errors nil)

(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

(setq backup-directory-alist `(("." . "~/.saves")))

(setq indent-tabs-mode t)
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

(setq-default fill-column 72)
(put 'dired-find-alternate-file 'disabled nil)

;; modes
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(line-number-mode -1)
(global-linum-mode 1)

(if (boundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode))

(vz/delete-mouse-selection-mode 1)

(global-visual-line-mode 1)

;; keybinds
(define-key key-translation-map (kbd "M-c") (kbd "C-g"))

(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

(global-set-key (kbd "M-o") 'open-line-above)

(global-set-key (kbd "<f9>") 'linum-mode)
(global-set-key (kbd "<f10>") 'writeroom-mode)

(windmove-default-keybindings)

;; hooks
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
