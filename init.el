(setq spacemacs-start-directory "~/etc/spacemacs/.emacs.d/")
;; (setq spacemacs-start-directory "~/.emacs.d/")
(setq package-user-dir (file-name-as-directory
                        (concat spacemacs-start-directory "elpa/")))
(load-file (concat spacemacs-start-directory "init.el"))
