(setq lib-file (concat user-emacs-directory "lib.el"))
(setq settings-file (concat user-emacs-directory "settings.el"))
(setq packages-file (concat user-emacs-directory "packages.el"))

(if (file-exists-p lib-file)
    (load-file lib-file))

(if (file-exists-p settings-file)
    (load-file settings-file))

(if (file-exists-p packages-file)
    (load-file packages-file))

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
