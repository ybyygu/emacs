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
(setq display-line-numbers-type t)

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
(setq persp-auto-save-opt 0)

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

;; [[file:../../doom.note::e9e282a0][e9e282a0]]
(load! "ui")
;; e9e282a0 ends here

;; [[file:../../doom.note::*edit][edit:1]]
(load! "edit")
;; edit:1 ends here

;; [[file:../../doom.note::*dired][dired:1]]
(use-package dired
  :config
  ;; Set this variable to non-nil, Dired will try to guess a default
  ;; target directory. This means: if there is a dired buffer
  ;; displayed in the next window, use its current subdir, instead
  ;; of the current subdir of this dired buffer. The target is used
  ;; in the prompt for file copy, rename etc.
  (progn
    (setq dired-dwim-target t)

    ;; Dired listing switches
    ;;  -a : Do not ignore entries starting with .
    ;;  -l : Use long listing format.
    ;;  -G : Do not print group names like 'users'
    ;;  -h : Human-readable sizes like 1K, 234M, ..
    ;;  -v : Do natural sort .. so the file names starting with . will show up first.
    ;;  -F : Classify filenames by appending '*' to executables,
    ;;       '/' to directories, etc.
    (setq dired-listing-switches "-alGhvF --group-directories-first") ; default: "-al"

    ;; 用于在dired中复制当前文件的全路径.
    (defun gwp/dired-copy-file-path()
      (interactive)
      (let ((current-prefix-arg '(0)))
        (call-interactively 'dired-copy-filename-as-kill)
        ))

    (map! :map dired-mode-map
          :localleader
          :desc "Copy file path"
          :n "y" #'gwp/dired-copy-file-path
          :desc "Make symlink"
          :n "l" #'dired-do-symlink
          :desc "Async shell command"
          :n "!" #'dired-do-async-shell-command
          )

    ;; 使用BACKSPACE来上一级目录, 使用Ctrl-shift-n来新建目录(默认为"+")
    (map! :map dired-mode-map
          :nv "DEL"   #'dired-up-directory       ; BACKSPACE
          :nv "C-S-n" #'dired-create-directory
          )
    ))
;; dired:1 ends here

;; [[file:../../doom.note::*dired][dired:2]]
(use-package dired-x
  :config
  (progn
    (setq dired-omit-verbose t)
    ;; (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))
  )
;; dired:2 ends here

;; [[file:../../doom.note::*org][org:1]]
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes/")
(setq org-roam-directory "~/Notes/roam")
(setq org-roam-file-extensions '("note" "org"))

;;
(use-package! org
  :config
  (load! "org")
  )

;; https://github.com/org-roam/org-roam-ui#doom
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  :hook (org-roam . org-roam-ui-mode)
  :config
  )
;; org:1 ends here

;; [[file:../../doom.note::*open-file-externally][open-file-externally:1]]
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
                  (call-process program nil 0 nil file-path)
                  )
                )
            (spacemacs/open-in-external-app file-path)
            )
        (message "No file associated to this buffer.")))))
;; open-file-externally:1 ends here

;; [[file:../../doom.note::*open in terminal][open in terminal:1]]
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
    (gwp/open-in-gnome-terminal (expand-file-name default-directory))
    ))
;; open in terminal:1 ends here

;; [[file:../../doom.note::*develop][develop:1]]
(load! "develop")
;; develop:1 ends here

;; [[file:../../doom.note::*recent files][recent files:1]]
(require 'recentf)
;; the default is only 20
(setq recentf-max-saved-items 1000)
(add-to-list 'recentf-exclude "autosave$")
(add-to-list 'recentf-exclude "\.png$")
(add-to-list 'recentf-exclude "\.pdf$")
(add-to-list 'recentf-exclude "\.svg$")
(add-to-list 'recentf-exclude "\.odt$")
;; recent files:1 ends here

;; [[file:../../doom.note::*fcitx][fcitx:1]]
(use-package! fcitx
  :after evil
  :config
  (when (executable-find "fcitx-remote")
    ;; (fcitx-prefix-keys-add "M-m")
    ;; 影响搜索界面, 不应该开
    ;; (setq fcitx-use-dbus t)
    (fcitx-aggressive-setup)
    ))
;; fcitx:1 ends here

;; [[file:../../doom.note::*dired-sidebar][dired-sidebar:1]]
(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar))
;; dired-sidebar:1 ends here

;; [[file:../../doom.note::24c9210c][24c9210c]]
(load! "bindings")
;; 24c9210c ends here
