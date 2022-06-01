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
(use-package acme-mouse
  :init
  (delete-selection-mode -1))

;; [package] magit
(use-package magit)

;; [package] vertico: better selection buffer
(use-package vertico
  :init
  (vertico-mode))

;; [config] disable electric indent
(electric-indent-mode -1)
(add-hook 'after-change-major-mode-hook (lambda()
					  (electric-indent-mode -1)))

;; [package] pdf-tools: opens pdfs
(use-package pdf-tools)

;; [package] rust-mode: mode for rust syntax highlighting
(use-package rust-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

;; [package] hare-mode: mode for hare
(use-package hare-mode
  :straight (hare-mode :type git :host nil
		       :repo "https://git.sr.ht/~bbuccianti/hare-mode")
  :init
  (add-to-list 'auto-mode-alist '("\\.ha\\'" . hare-mode)))

;; [package] tex: latex stuff
(use-package tex
  :straight auctex
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

(use-package org-fragtog
  :init
  (add-hook 'org-mode-hook 'org-fragtog-mode))

;; [package] tmr: useful timers (finally, because I used to use my phone
;; previously)
(use-package tmr
  :straight (tmr :type git :host nil
		 :repo "https://git.sr.ht/~protesilaos/tmr"))

;; [package] plz: (dep) ement
(use-package plz
  :straight (plz :type git :host nil
		   :repo "https://github.com/alphapapa/plz.el"))

;; [package] ement: matrix client
(use-package ement
  :straight (ement :type git :host nil
		   :repo "https://github.com/alphapapa/ement.el"))

;; [config] set modus-vivendi theme
(use-package modus-themes
  :init
  (modus-themes-load-themes)
  (modus-themes-load-vivendi))

;; [config] display line numbers
(global-display-line-numbers-mode 1)

;; [config] disable menu-bar-mode  scroll-bar and menu-bar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; [config] more natural scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(pixel-scroll-precision-mode)

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
 '(custom-safe-themes
   '("02fff7eedb18d38b8fd09a419c579570673840672da45b77fde401d8708dc6b5" default))
 '(ement-notify-notification-predicates
   '(ement-notify--event-mentions-session-user-p ement-notify--event-mentions-room-p))
 '(fill-nobreak-predicate '(fill-french-nobreak-p)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 120 :width normal :foundry "PfEd" :family "Go Mono"))))
 '(fringe ((t (:inherit background)))))


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

(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 4
				  tab-width 4
				  indent-tabs-mode nil)))

(put 'dired-find-alternate-file 'disabled nil)

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

(vz/delete-mouse-selection-mode 1)

(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(windmove-default-keybindings)
