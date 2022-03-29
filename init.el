;; Disable package.el
(setq package-enable-at-startup nil)

;; straight.el booststrap (https://github.com/raxod502/straight.el#getting-started)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; [package] use-package
(straight-use-package 'use-package)

;; [config] straight: use use-package by default
(use-package straight
  :custom (straight-use-package-by-default t))

;; [package] acme-mouse
(use-package acme-mouse)

;; [package] magit
(use-package magit)

;; [package] dired-sidebar
(use-package dired-sidebar
  :bind
  (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

;; [package] vscode-icon
(use-package vscode-icon
  :commands (vscode-icon-for-file))

;; [package] vertico: better selection buffer
(use-package vertico
  :init
  (vertico-mode))

;; [package] pdf-tools: opens pdfs
(use-package pdf-tools)

;; [config] linum
(global-linum-mode 1)

;; [config] disable menu-bar-mode  scroll-bar and menu-bar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; [config] more natural scrolling (a bit janky until pixel perfect scrolling in 29)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; [config] highlighting
(global-hl-line-mode 1)

;; [config] backups in fixed directory
(setq backup-directory-alist `(("." . "~/.saves")))

;; [config] keyboard exit modal: add M-c as alias for C-g
(define-key key-translation-map (kbd "M-c") (kbd "C-g"))

;; [INTERFACE] set with the custom interface
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "    " :family "GoMono Nerd Font"))))
 '(fringe ((t (:inherit background))))
 '(highlight ((t (:background "gray80"))))
 '(region ((t (:extend t :background "dark sea green" :distant-foreground "gtk_selection_fg_color")))))

;; [command] open-line-above: mimicks vim shift+o behavior
(defun open-line-above ()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-o") 'open-line-above)

;; [config] set tabs
(setq indent-tabs-mode t)
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
