;; [[file:~/Install/configs/spacemacs/config.note::0bf61789-4b47-43e0-92a7-474d6bb4595d][0bf61789-4b47-43e0-92a7-474d6bb4595d]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  File:       ~/.spacemacs.d/layers/gwp/packages.el
;;  Created:    <2017-10-09 Mon>
;;  UPDATED:    <2017-10-24 Tue 20:37>
;;  Platform:   Emacs (Spacemacs)
;;  Author:     Wenping Guo <ybyygu@gmail.com>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0bf61789-4b47-43e0-92a7-474d6bb4595d ends here

;; [[file:~/Install/configs/spacemacs/config.note::98f62082-87ca-47b9-aba5-86e2210e0705][98f62082-87ca-47b9-aba5-86e2210e0705]]
;;; packages.el --- gwp layer packages file for Spacemacs.
;;  Time-stamp: <2017-03-06 23:37:14 ybyygu>

;;; Commentary:

;; Briefly, each package to be installed or configured by this layer should be
;; added to `gwp-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `gwp/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `gwp/pre-init-PACKAGE' and/or
;;   `gwp/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst gwp-packages
  '(
    (vc :location built-in)
    (org)                      ;; org-plus-contrib
    (org-download :excluded t) ;; shipped with spacemacs in org layer
    (hl-todo :excluded t)      ;; shipped with spacemacs in spacemacs-ui-visual layer
    cnfonts                    ;; chinese fonts setup
    cal-china-x
    ox-latex-chinese
    ob-ipython
    org-attach-screenshot
    fcitx
    )

  "The list of Lisp packages required by the gwp layer.")
;; 98f62082-87ca-47b9-aba5-86e2210e0705 ends here

;; [[file:~/Install/configs/spacemacs/config.note::3936797b-171a-4257-b96e-c28ad0cec464][3936797b-171a-4257-b96e-c28ad0cec464]]
(defun gwp/init-cal-china-x ()
  (use-package calendar
    :config
    (progn
      (setq calendar-week-start-day 1)
      (setq calendar-mark-holidays-flag t)
      )
    )

  (use-package cal-china-x
    :config
    (progn
      (setq local-holidays '(
                             (holiday-lunar 12 30  "春节" 0)
                             (holiday-lunar 1 15     "元宵" 0)
                             (holiday-lunar 5 5  "端午" 0)
                             (holiday-lunar 7 7  "七夕" 0)
                             (holiday-lunar 8 15     "中秋" 0)
                             (holiday-lunar 8 25     "焱焱生日" 0)
                             (holiday-lunar 12 23     "金玉生日" 0)
                             (holiday-lunar 12 14     "文华生日" 0)
                             (holiday-lunar 2 3     "二毛生日" 0)
                             (holiday-lunar 9 9  "重阳" 0)
                             (holiday-lunar 12 8     "腊八" 0)
                             (holiday-lunar 12 24    "小年" 0)))

      (setq calendar-holidays local-holidays)
      (setq calendar-holidays cal-china-x-important-holidays)
      )
    )
  )
;; 3936797b-171a-4257-b96e-c28ad0cec464 ends here

;; [[file:~/Install/configs/spacemacs/config.note::2f729bdf-5700-4f80-a94f-e73113348e08][2f729bdf-5700-4f80-a94f-e73113348e08]]
(defun gwp/init-vc ()
  (use-package vc
    :init
    (progn
      (add-hook 'vc-before-checkin-hook #'time-stamp)
      )
    )

  (use-package vc-hooks
    :init
    (progn
      ;; Don't ask if I want to visit a sym-linked file under VC. I always want to!
      (setq vc-follow-symlinks t)
      )
    )
  )
;; 2f729bdf-5700-4f80-a94f-e73113348e08 ends here

;; [[file:~/Install/configs/spacemacs/config.note::d1097c8f-3faa-4853-8786-d9d0d9c04575][d1097c8f-3faa-4853-8786-d9d0d9c04575]]
(defun gwp/init-fcitx ()
  (use-package fcitx
    :init
    (setq fcitx-active-evil-states '(insert emacs hybrid)) ; if you use hybrid mode
    ;; M-m is common in Spacemacs
    (fcitx-prefix-keys-add "M-m")

    ;; for Linux users, it is recommended
    (fcitx-aggressive-setup)
    (setq fcitx-use-dbus t)
    )
  )
;; d1097c8f-3faa-4853-8786-d9d0d9c04575 ends here

;; [[file:~/Install/configs/spacemacs/config.note::8b9c6195-c08c-44d8-b5c6-2698402f3cb6][8b9c6195-c08c-44d8-b5c6-2698402f3cb6]]
(defun gwp/init-org-attach-screenshot ()
  (use-package org-attach-screenshot
               :after org
               :bind
               (("C-c <insert>" . org-attach-screenshot))
               :config
               (setq org-attach-screenshot-command-line "deepin-screenshot -s %f")
               )
  )
;; 8b9c6195-c08c-44d8-b5c6-2698402f3cb6 ends here

;; [[file:~/Install/configs/spacemacs/config.note::3ef398cc-9c73-4978-b31c-35f2aa476c44][3ef398cc-9c73-4978-b31c-35f2aa476c44]]
(defun gwp/init-cnfonts ()
  ;; chinese-fonts-setup is amazing
  (use-package cnfonts
    :ensure t
    :config
    (progn
      (setq cnfonts-profiles
            '("program" "org-mode" "read-book"))
      (setq cnfonts-profiles-directory (expand-file-name
                                        "chinese-fonts-setup"
                                        dotspacemacs-directory))
      (setq cnfonts--current-profile-name "program")
      (setq cnfonts-use-face-font-rescale t)
      )
    (cnfonts-enable)
    (cnfonts-set-spacemacs-fallback-fonts)
    )
  )
;; 3ef398cc-9c73-4978-b31c-35f2aa476c44 ends here

;; [[file:~/Install/configs/spacemacs/config.note::8d9d4320-8a15-4b4b-8b4d-40c58b895804][8d9d4320-8a15-4b4b-8b4d-40c58b895804]]
(defun gwp/post-init-org ()
  (with-eval-after-load 'org
    (progn
      (add-to-list 'auto-mode-alist '("\\.note\\'" . org-mode))
      (add-to-list 'auto-mode-alist '("NOTE" . org-mode))

      (setq org-directory  "~/Notes")
      (setq org-default-notes-file (concat org-directory "/life.note"))
      (setq org-blank-before-new-entry nil)

      ;; all other things
      (load-file "~/.spacemacs.d/init-org.el")
      )
    )
  )

(defun gwp/init-ox-latex-chinese ()
  ;;
  )

(defun gwp/init-ob-ipython ()
  ;;
  (use-package ob-ipython
    :defer t
    )
  )
;; 8d9d4320-8a15-4b4b-8b4d-40c58b895804 ends here
