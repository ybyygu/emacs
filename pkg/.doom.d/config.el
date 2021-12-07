;; [[file:../../doom.note::b70e7222][b70e7222]]
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Wenping Guo"
      user-mail-address "ybyygu@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "Monaco" :size 13))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
;; b70e7222 ends here

;; [[file:../../doom.note::11b27926][11b27926]]
(setq doom-scratch-initial-major-mode 'emacs-lisp)
;; 11b27926 ends here

;; [[file:../../doom.note::6b10b827][6b10b827]]
(after! persp-mode
  (setq persp-auto-save-opt 0))

(defun gwp/workspace/load-or-switch (name)
  "Load or switch to a workspace."
  (interactive
   (list
    (completing-read
     "Workspace to load: "
     (persp-list-persp-names-in-file
      (expand-file-name +workspaces-data-file persp-save-dir)))))
  (if (+workspace-exists-p name)
      (+workspace/switch-to name)
    (if (not (+workspace-load name))
        (+workspace-error (format "Couldn't load workspace %s" name))
      (+workspace/switch-to name)))
  (+workspace/display))
;; 6b10b827 ends here

;; [[file:../../doom.note::e13c7903][e13c7903]]
(defun spacemacs/open-in-external-app (file-path)
  "Open `file-path' in external application."
  (let ((process-connection-type nil))
    (start-process "" nil "xdg-open" file-path)))

(defun spacemacs/open-file-or-directory-in-external-app (arg)
  "Open current file in external application.
If the universal prefix argument is used then open the folder
containing the current file by the default explorer.
If two universal prefix arguments are used, then prompt for command to use."
  (interactive "P")
  (if (equal arg '(4))                  ; C-u
      (spacemacs/open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (if (equal arg '(16))         ; C-u C-u
              (progn
                (let ((program (read-shell-command "Open current file with: ")))
                  (call-process program nil 0 nil file-path)))
            (spacemacs/open-in-external-app file-path))
        (message "No file associated to this buffer.")))))
;; e13c7903 ends here

;; [[file:../../doom.note::12a811d1][12a811d1]]
(defun gwp/open-in-gnome-terminal (the-directory)
  "Open `the-directory' in external gnome-terminal."
  (let ((process-connection-type nil))
    ;; (start-process "" nil "terminal-dwim.sh" (concat "--working-directory=" the-directory) "-e" "tmux")
    (start-process "" nil "alacritty" (concat "--working-directory=" the-directory) "-e" "tmux")
    ))

(defun gwp/open-terminal-here ()
  "Open the current dir in a new terminal window"
  (interactive)
  (let ((default-directory (or (and (eq major-mode 'dired-mode)
                                    (dired-current-directory))
                               default-directory)))
    (gwp/open-in-gnome-terminal (expand-file-name default-directory))))
;; 12a811d1 ends here

;; [[file:../../doom.note::493c2a26][493c2a26]]
(use-package recentf
  :custom
  ;; then run M-x recentf-cleanup to make it work.
  (recentf-exclude '("/tmp/"
                     "/ssh:"
                     "/sudo:"
                     "/scp:"
                     "/scpx:"
                     "/ssh:"
                     ;; "\\.pdf$"
                     "\\.png$"
                     "autosave$"
                     ;; "\\.odt$"
                     "\\.note_archive$"
                     "_workspaces"
                     ".*/COMMIT_EDITMSG$" ; magit 临时编辑文件
                     ;; ".*/$"               ; 剔除目录
                     ))
  (recentf-max-saved-items 9999)   ; the default is only 20
  (recentf-keep '(gwp::recentf-keep-p))
  ;; clean up items when has been idle 1 hour
  ;; (recentf-auto-cleanup 3600)
  (recentf-auto-cleanup 'never)         ; doom 在退出时清理
  )

(defun gwp::recentf-keep-p (file)
  "仅保留本地可读文件"
  (not (file-remote-p file))
  ;; (and (not (file-remote-p file))
  ;;      (not (file-directory-p file)))
  )
;; 493c2a26 ends here

;; [[file:../../doom.note::38a0a087][38a0a087]]
(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar))
;; 38a0a087 ends here

;; [[file:../../doom.note::c54f13b5][c54f13b5]]
(require 'init-proxy)
(require 'init-magit)
;; (require 'init-dired)
;; (require 'init-search)
;; (require 'init-eaf)
;; c54f13b5 ends here
