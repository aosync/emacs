;; require: package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; setq-default sets a variable for all buffers
;; I am now setting the indentation mode to tab for all buffers
(setq-default c-basic-offset 8
	      tab-width 8
	      indent-tabs-mode t)

;; hooks: python 4 spaces
(add-hook 'python-mode-hook
	  (progn
	    (setq indent-tabs-mode nil)
	    (setq python-indent-offset 4)))

;; Disable the tool-bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; set all backups in a special directory instead of everywhere
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; key: set C-x k to kill buffer without confirmation
(global-set-key [(control x) (k)] 'kill-this-buffer)

;; key: set C-g to C-z
(global-set-key [(control z)] 'keyboard-quit)

;; mod: load acme-mouse
(load-library "~/.emacs.d/acme-mouse.el")

;; setup melpa
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#eef2f7" :foreground "#242626" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "    " :family "Go Mono")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(undo-tree magit projectile ## auto-package-update)))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package smart-tabs-mode
  :config
  (smart-tabs-insinuate 'c 'c++ 'java))

(use-package guru-mode
  :config
  (guru-global-mode +1))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (progn
    (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
    (setq company-idle-delay 0)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((c++-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package projectile
  :config
  (setq projectile-project-search-path '("~/Repos"))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package magit)
(use-package undo-tree
  :config
  (global-undo-tree-mode))
