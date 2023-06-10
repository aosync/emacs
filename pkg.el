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

(use-package tex
  :straight auctex
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

(use-package org-fragtog
  :init
  (add-hook 'org-mode-hook 'org-fragtog-mode))

(use-package tmr
  :straight (tmr :type git :host nil
		 :repo "https://git.sr.ht/~protesilaos/tmr"))

(use-package plz
  :straight (plz :type git :host nil
		   :repo "https://github.com/alphapapa/plz.el"))

(use-package ement
  :straight (ement :type git :host nil
		   :repo "https://github.com/alphapapa/ement.el"))

(use-package neotree
  :config
  (global-set-key [f8] 'neotree-toggle))

(use-package writeroom-mode)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (c-mode . lsp-mode))

(use-package company-mode
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
		("<tab>" . company-complete-selection))
  (:map lsp-mode-map
		("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package pdf-tools)

(use-package rust-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(use-package hare-mode
  :straight (hare-mode :type git :host nil
		       :repo "https://git.sr.ht/~bbuccianti/hare-mode")
  :init
  (add-to-list 'auto-mode-alist '("\\.ha\\'" . hare-mode)))

(use-package acme-mouse
  :init
  (delete-selection-mode -1))

(use-package magit)

(use-package vertico
  :init
  (vertico-mode))
