;; [[file:~/Install/configs/spacemacs/config.note::f457b32b-bcfd-4618-b7d4-e2e83399037e][f457b32b-bcfd-4618-b7d4-e2e83399037e]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  File:       ~/.spacemacs.d/layers/gwp/config.el
;;  Created:    <2017-10-09 Mon>
;;  UPDATED:    <2018-01-22 Mon 16:23>
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

;; [[file:~/Install/configs/spacemacs/config.note::fc2c78fb-3f6d-47c9-9b92-60cee197beb4][fc2c78fb-3f6d-47c9-9b92-60cee197beb4]]
(setq user-mail-address "ybyygu@gmail.com")
(setq default-tab-width 4)
(setq tab-width 4)                             ; Length of tab is 4 SPC
;; fc2c78fb-3f6d-47c9-9b92-60cee197beb4 ends here

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

;; [[file:~/Install/configs/spacemacs/config.note::9dfdbbc5-231c-433c-a756-02c7ac96aeb9][9dfdbbc5-231c-433c-a756-02c7ac96aeb9]]
(defun gwp/open-in-gnome-terminal (the-directory)
  "Open `the-directory' in external gnome-terminal."
  (let ((process-connection-type nil))
    (start-process "" nil "gnome-terminal" (concat "--working-directory=" the-directory))
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
;; 9dfdbbc5-231c-433c-a756-02c7ac96aeb9 ends here

;; [[file:~/Install/configs/spacemacs/config.note::7f135717-84cd-444e-8946-c96a9c2429f1][7f135717-84cd-444e-8946-c96a9c2429f1]]
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
;; 7f135717-84cd-444e-8946-c96a9c2429f1 ends here

;; [[file:~/Install/configs/spacemacs/config.note::b50b9af5-8011-44e9-b278-47b71ac0a7d8][b50b9af5-8011-44e9-b278-47b71ac0a7d8]]
(setq profiler-report-cpu-line-format '((150 left) (24 right ((19 right) (5 right)))))
;; b50b9af5-8011-44e9-b278-47b71ac0a7d8 ends here
