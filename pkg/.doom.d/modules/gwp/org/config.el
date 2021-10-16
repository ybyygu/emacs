;; [[file:../../../../../doom.note::abd5e254][abd5e254]]
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes/")
(setq org-roam-directory "~/Notes/roam")
(setq org-roam-file-extensions '("note" "org"))

(use-package! org
  :config
  (load! "org"))

;; https://github.com/org-roam/org-roam-ui#doom
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  :hook (org-roam . org-roam-ui-mode)
  :config)
;; abd5e254 ends here
