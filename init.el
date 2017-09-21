(setq spacemacs-start-directory "~/Install/config/spacemacs/.emacs.d/")
(setq package-user-dir (file-name-as-directory
                        (concat spacemacs-start-directory "elpa/")))
(load-file (concat spacemacs-start-directory "init.el"))
