;; [[file:../../../../../doom.note::abd5e254][abd5e254]]
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes/")
(setq org-roam-directory "~/Notes/roam")
(setq org-roam-file-extensions '("note" "org"))

(use-package! org
  :init
  ;; treat .note files as org-mode
  (add-to-list 'auto-mode-alist '("\\.note\\'" . org-mode))
  (add-to-list 'auto-mode-alist '("NOTE" . org-mode))
  :config
  (load! "org"))

;; https://github.com/org-roam/org-roam-ui#doom
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  :hook (org-roam . org-roam-ui-mode)
  :config)

(use-package! evil-org-agenda
  :hook (org-agenda-mode . evil-org-agenda-mode)
  :config
  (evil-org-agenda-set-keys)
  (evil-define-key* 'motion evil-org-agenda-mode-map
    (kbd doom-leader-key) nil))

(use-package! init-note)
;; abd5e254 ends here
