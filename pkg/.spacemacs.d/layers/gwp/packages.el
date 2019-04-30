;; [[file:~/Install/configs/spacemacs/config.note::*header][header:1]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  File:       ~/.spacemacs.d/layers/gwp/packages.el
;;  Created:    <2017-10-09 Mon>
;;  UPDATED:    <2019-04-30 Tue 15:16>
;;  Platform:   Emacs (Spacemacs)
;;  Author:     Wenping Guo <ybyygu@gmail.com>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; header:1 ends here

;; [[file:~/Install/configs/spacemacs/config.note::*define%20packages][define packages:1]]
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
    (hl-todo :excluded t)      ;; shipped with spacemacs in spacemacs-ui-visual layer
    cnfonts                    ;; chinese fonts setup
    cal-china-x
    ox-latex-chinese
    ;; org-attach-screenshot   ;; disabled since 2017-11-17
    fcitx
    interleave                 ;; for PDF annotations
    bm                         ;; for visual bookmarks
    )

  "The list of Lisp packages required by the gwp layer.")
;; define packages:1 ends here

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
                             (holiday-lunar 9 9  "重阳" 0)
                             (holiday-lunar 12 8     "腊八" 0)
                             (holiday-lunar 12 24    "小年" 0)))

      (setq calendar-holidays local-holidays)
      (setq calendar-holidays cal-china-x-important-holidays)
      )
    )
  )
;; 3936797b-171a-4257-b96e-c28ad0cec464 ends here

;; [[file:~/Install/configs/spacemacs/config.note::*init%20vc][init vc:1]]
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
;; init vc:1 ends here

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

;; [[file:~/Install/configs/spacemacs/config.note::*init%20cnfonts][init cnfonts:1]]
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
;; init cnfonts:1 ends here

;; [[file:~/Install/configs/spacemacs/config.note::*init%20org][init org:1]]
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
;; init org:1 ends here

;; [[file:~/Install/configs/spacemacs/config.note::*init%20interleave][init interleave:1]]
(defun gwp/init-interleave ()
  (use-package interleave
    :ensure t
    )
  )
;; init interleave:1 ends here

;; [[file:~/Install/configs/spacemacs/config.note::*init%20bm][init bm:1]]
(defun gwp/init-bm ()
  (defun gwp-mouse-toggle-bm (e)
    "Toggle bookmarking
This command should be bound to a mouse key.
Argument E is a mouse event used by `mouse-set-point'."
    (interactive "@e")
    (save-excursion
      (mouse-set-point e)
      (bm-toggle)
      )
    )

  ;; adopted from: https://github.com/joodland/bm
  (use-package bm
    :ensure t
    :demand t

    :init
    ;; restore on load (even before you require bm)
    (setq bm-restore-repository-on-load t)

    :config
    ;; Allow cross-buffer 'next'
    (setq bm-cycle-all-buffers nil)

    ;; save bookmarks
    (setq-default bm-buffer-persistence t)

    ;; Loading the repository from file when on start up.
    (add-hook 'after-init-hook 'bm-repository-load)

    ;; Saving bookmarks
    (add-hook 'kill-buffer-hook #'bm-buffer-save)

    ;; Saving the repository to file when on exit.
    ;; kill-buffer-hook is not called when Emacs is killed, so we
    ;; must save all bookmarks first.
    (add-hook 'kill-emacs-hook #'(lambda nil
                                   (bm-buffer-save-all)
                                   (bm-repository-save)))

    ;; The `after-save-hook' is not necessary to use to achieve persistence,
    ;; but it makes the bookmark data in repository more in sync with the file
    ;; state.
    (add-hook 'after-save-hook #'bm-buffer-save)

    ;; Restoring bookmarks
    (add-hook 'find-file-hooks   #'bm-buffer-restore)
    (add-hook 'after-revert-hook #'bm-buffer-restore)

    ;; The `after-revert-hook' is not necessary to use to achieve persistence,
    ;; but it makes the bookmark data in repository more in sync with the file
    ;; state. This hook might cause trouble when using packages
    ;; that automatically reverts the buffer (like vc after a check-in).
    ;; This can easily be avoided if the package provides a hook that is
    ;; called before the buffer is reverted (like `vc-before-checkin-hook').
    ;; Then new bookmarks can be saved before the buffer is reverted.
    ;; Make sure bookmarks is saved before check-in (and revert-buffer)
    (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

    :bind (
            ([left-fringe mouse-3] . gwp-mouse-toggle-bm)
            ([left-margin mouse-3] . gwp-mouse-toggle-bm)
            ([left-fringe mouse-5] . bm-next-mouse)
            ([left-margin mouse-5] . bm-next-mouse)
            ([left-fringe mouse-4] . bm-previous-mouse)
            ([left-margin mouse-4] . bm-previous-mouse)
            )
    )
  )
;; init bm:1 ends here
