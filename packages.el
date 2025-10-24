;; packages.el: specifies and configures packages

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

;; configure use package 
(straight-use-package 'use-package)

;; packages
(use-package straight
  :custom (straight-use-package-by-default t))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode 1))

(use-package magit)

(use-package vterm
  :ensure t
  :hook
  (vterm-mode . (lambda ()
				  (display-line-numbers-mode 0))))

(use-package latex
  :straight auctex
  :hook ((LaTeX-mode . LaTeX-math-mode)
	 (LaTeX-mode . turn-on-reftex)
	 (LaTeX-mode . TeX-source-correlate-mode)
	 (LaTeX-mode . (lambda ()
			 (unless (file-directory-p "bld")
			   (make-directory "bld")))))
  :hook
  (LaTeX-mode . visual-line-mode)
  (LaTeX-mode . TeX-source-correlate-mode)
  :custom
  (TeX-command-extra-options "-shell-escape -synctex=1")
  (TeX-output-dir "bld")
  (TeX-auto-save  t)
  (TeX-parse-self t)
  (TeX-master     nil)
  (TeX-source-correlate-start-server t)
  (TeX-view-program-selection
   '((output-pdf  "Zathura")
	 (output-html "firefox"))))

(use-package org-fragtog
  :hook ('org-mode-hook 'org-mode-fragtog-mode))

(use-package treemacs
  :ensure t
  :bind (("C-x t t" . treemacs))
  :config
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-git-mode 'deferred))

(use-package treemacs-evil
  :after (treemacs evil))

;; (use-package neotree
;;   :config
;;   (global-set-key [f8] 'neotree-toggle))

(use-package writeroom-mode)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (c-mode . lsp-mode))

(use-package company-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
		("<tab>" . company-complete-selection))
  (:map lsp-mode-map
		("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package pdf-tools)

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package go-mode)

(use-package vertico
  :config
  (vertico-mode))

(use-package sly
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(use-package rainbow-delimiters
  :hook (('lisp-mode . 'rainbow-delimiters-mode)
	 ('emacs-lisp-mode-hook 'rainbow-delimiters-mode)))
