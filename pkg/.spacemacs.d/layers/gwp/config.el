;; [[file:~/Install/configs/spacemacs/config.note::f457b32b-bcfd-4618-b7d4-e2e83399037e][f457b32b-bcfd-4618-b7d4-e2e83399037e]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  File:       ~/.spacemacs.d/layers/gwp/config.el
;;  Created:    <2017-10-09 Mon>
;;  UPDATED:    <2019-04-30 Tue 14:13>
;;  Platform:   Emacs (Spacemacs)
;;  Author:     Wenping Guo <ybyygu@gmail.com>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; f457b32b-bcfd-4618-b7d4-e2e83399037e ends here

;; [[file:~/Install/configs/spacemacs/config.note::371dccb7-b33d-4674-8e09-64fa1d9806ab][371dccb7-b33d-4674-8e09-64fa1d9806ab]]
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(global-font-lock-mode t)

;; force a syntax-highlighting refresh
(global-set-key (kbd "<f5> <f5>") 'font-lock-fontify-buffer)
(auto-image-file-mode t)              ; View images inside Emacs
;; set line space wider than default
(setq-default line-spacing 4)

;; 修改 frame 标题 方便 gnome-shell 桌面切换
(setq frame-title-format '("" "%b: " buffer-file-name))
;; 371dccb7-b33d-4674-8e09-64fa1d9806ab ends here

;; [[file:~/Install/configs/spacemacs/config.note::e7ef737b-dd78-4ba0-94e6-d63dc64ebb24][e7ef737b-dd78-4ba0-94e6-d63dc64ebb24]]
;; emacs scrolls too fast
;; http://stackoverflow.com/questions/445873/emacs-mouse-scrolling
(setq mouse-wheel-scroll-amount '(1 ((Shift) . 2) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)      ; constant speed
(setq scroll-preserve-screen-position nil)    ; do not reposition the screen when moving the cursor
;; e7ef737b-dd78-4ba0-94e6-d63dc64ebb24 ends here

;; [[file:~/Install/configs/spacemacs/config.note::*tab%20key][tab key:1]]
(setq user-mail-address "ybyygu@gmail.com")
(setq default-tab-width 4)
(setq tab-width 4)                             ; Length of tab is 4 SPC
;; tab key:1 ends here

;; [[file:~/Install/configs/spacemacs/config.note::*bookmark][bookmark:1]]
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

;; bookmark:1 ends here

;; [[file:~/Install/configs/spacemacs/config.note::139c7654-b2cd-4c3c-9b37-a5ee6a64aef4][139c7654-b2cd-4c3c-9b37-a5ee6a64aef4]]
;; (defun gwp/quit-frame-and-kill-buffer ()
;;   "kill the current buffer and the current frame"
;;   (interactive)

;;   (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
;;   (kill-buffer)
;;   (delete-frame)
;;   )

;; quit emacs server
;; (global-set-key (kbd "<C-f4>") 'save-buffers-kill-emacs)
;; 139c7654-b2cd-4c3c-9b37-a5ee6a64aef4 ends here

;; [[file:~/Install/configs/spacemacs/config.note::aff9dc6c-0938-4b49-ad3d-4fe70f1f7bb0][aff9dc6c-0938-4b49-ad3d-4fe70f1f7bb0]]
;; http://stackoverflow.com/questions/268088/how-to-remove-the-prompt-for-killing-emacsclient-buffers
;; this seems work
(defalias 'server-kill-buffer-query-function '(lambda () t))
;; aff9dc6c-0938-4b49-ad3d-4fe70f1f7bb0 ends here

;; [[file:~/Install/configs/spacemacs/config.note::e4c5c4a4-3c01-4361-b375-73b81af1ed18][e4c5c4a4-3c01-4361-b375-73b81af1ed18]]
(setq abbrev-file-name (expand-file-name
                        "abbreviations"
                        dotspacemacs-directory))

(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))
(setq save-abbrevs 'silently)
;; e4c5c4a4-3c01-4361-b375-73b81af1ed18 ends here

;; [[file:~/Install/configs/spacemacs/config.note::33866aac-efb3-45f7-bfc2-f450db84c76f][33866aac-efb3-45f7-bfc2-f450db84c76f]]
(require 'recentf)
;; the default is only 20
(setq recentf-max-saved-items 1000)
(add-to-list 'recentf-exclude "\.png$")
(add-to-list 'recentf-exclude "\.pdf$")
(add-to-list 'recentf-exclude "\.svg$")
(add-to-list 'recentf-exclude "\.odt$")
;; 33866aac-efb3-45f7-bfc2-f450db84c76f ends here

;; [[file:~/Install/configs/spacemacs/config.note::*open%20in%20terminal][open in terminal:1]]
(defun gwp/open-in-gnome-terminal (the-directory)
  "Open `the-directory' in external gnome-terminal."
  (let ((process-connection-type nil))
    (start-process "" nil "gnome-terminal" (concat "--working-directory=" the-directory))
    ;; (start-process "" nil "alacritty" (concat "--working-directory=" the-directory) "-e" "tmux")
    )
  )

(defun gwp/open-terminal-here ()
  "Open the current dir in a new terminal window"
  (interactive)
  (let ((default-directory (or (and (eq major-mode 'dired-mode)
                                    (dired-current-directory))
                               default-directory)))
    (gwp/open-in-gnome-terminal (expand-file-name default-directory))
    )
  )
;; open in terminal:1 ends here

;; [[file:~/Install/configs/spacemacs/config.note::*hide%20dotfiles][hide dotfiles:1]]
(require 'dired-x)
(setq-default dired-omit-files-p nil) ; Buffer-local variable
;; (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(setq dired-listing-switches "--group-directories-first -l -X")
;; hide dotfiles:1 ends here

;; [[file:~/Install/configs/spacemacs/config.note::*profiler][profiler:1]]
(setq profiler-report-cpu-line-format '((150 left) (24 right ((19 right) (5 right)))))
;; profiler:1 ends here

;; [[file:~/Install/configs/spacemacs/config.note::5c82f9a9-5afd-48e5-bbef-1603120eb3c5][5c82f9a9-5afd-48e5-bbef-1603120eb3c5]]
(setq magit-revision-show-gravatars nil)
;; 5c82f9a9-5afd-48e5-bbef-1603120eb3c5 ends here
