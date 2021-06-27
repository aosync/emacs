;; require: package
(require 'package)

;; set the font to Go Mono
(add-to-list 'default-frame-alist
	     '(font . "Go Mono-11"))

;; setq-default sets a variable for all buffers
;; I am now setting the indentation mode to tab for all buffers
(setq-default c-basic-offset 8
	      tab-width 8
	      indent-tabs-mode t)

;; hooks: python 4 spaces
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq python-indent-offset 4)))

;; Disable the tool-bar
(tool-bar-mode -1)
;; menu-bar-mode -> menu bar ; toggle-scroll-bar -> the scroll bar (i am fine with them
;; for now)

;; set all backups in a special directory instead of everywhere
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; key: set C-x k to kill buffer without confirmation
(global-set-key [(control x) (k)] 'kill-this-buffer)

;; key: set C-g to C-z
(global-set-key [(control z)] 'keyboard-quit)

;; mod: load acme-mouse
(load-library "~/.emacs.d/acme-mouse.el")

;; setup melpa
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(guru-mode smart-tabs-mode treemacs slime-volleyball)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#eef2f7" :foreground "#242626" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "    " :family "Go Mono")))))

(setq inferior-lisp-program "sbcl")
(smart-tabs-insinuate 'c 'java)
(treemacs)
(guru-global-mode +1)
