(setq custom-file (concat user-emacs-directory "custom.el"))
(load-file custom-file)

(setq lib-file (concat user-emacs-directory "lib.el"))
(setq settings-file (concat user-emacs-directory "packages.el"))
(setq packages-file (concat user-emacs-directory "settings.el"))

(if (file-exists-p lib-file)
    (load-file lib-file))

(if (file-exists-p settings-file)
    (load-file settings-file))

(if (file-exists-p packages-file)
    (load-file packages-file))

