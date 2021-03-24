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
