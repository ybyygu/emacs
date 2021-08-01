;; [[file:../../doom.note::*orig][orig:1]]
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
;; orig:1 ends here

;; [[file:../../doom.note::*workspace][workspace:1]]
(setq persp-auto-save-opt 0)
;; workspace:1 ends here

;; [[file:../../doom.note::*chinese fonts setup][chinese fonts setup:1]]
;; (use-package! cnfonts
;;   :config
;;   (progn
;;     (setq cnfonts-profiles
;;           '("program" "org-mode" "read-book"))
;;     (setq cnfonts-use-face-font-rescale t)
;;     )
;;   (cnfonts-enable)
;;   )

;; 这样modeline就正常了
;; https://emacs-china.org/t/doom-emacs/10390
(defun gwp/set-fonts()
  (interactive)
  (if (display-graphic-p)
      (progn
        ;; english font
        ;; (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "Monaco" 16)) ;; 11 13 17 19 23
        ;; (setq doom-font (font-spec :family "Monaco" :size 16))
        (setq doom-font (font-spec :family "Monaco"))
        ;; chinese font
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Adobe Heiti Std")))) ;; 14 16 20 22 28
    ))
(global-set-key (kbd "<f5> <f5>") 'gwp/set-fonts)

(defun gwp/init-fonts(frame)
  (with-selected-frame frame
    (if (display-graphic-p)
        (gwp/set-fonts))))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions #'gwp/init-fonts))

;; org-mode表格中文混排对齐
(after! org
  (call-interactively 'gwp/set-fonts)
  (custom-set-faces!
    `(org-table :family "Ubuntu Mono")
    )
  )
;; chinese fonts setup:1 ends here

;; [[file:../../doom.note::*input method][input method:1]]
(use-package! pyim
  :config
  (setq default-input-method "pyim")

  ;; 我使用五笔
  (use-package! pyim-wbdict
    :config (pyim-wbdict-v98-enable))
  (setq pyim-default-scheme 'wubi)

  ;; 启用拼音大词库，方便忘词用拼音反查
  (use-package pyim-basedict
    :config (pyim-basedict-enable))
  ;; 如果用户在使用五笔输入法的过程中，忘记了某个字的五笔码，可以按 TAB(F2-TAB)键临时切换到辅助输入法来输入，选词完成之后自动退出。
  (setq pyim-assistant-scheme 'quanpin)

  ;; 全角半角
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 不使用中文标点, 如需输入可切换至fcitx.
  ;; (delete '("/" "、")  pyim-punctuation-dict)
  ;; (add-to-list 'pyim-punctuation-dict '("\\" "、"))
  (setq pyim-punctuation-dict nil)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  ;; (setq-default pyim-english-input-switch-functions
  ;;               '(
  ;;                 ;; pyim-probe-dynamic-english
  ;;                 pyim-probe-isearch-mode
  ;;                 pyim-probe-program-mode
  ;;                 pyim-probe-org-structure-template
  ;;                 ))
  
  ;; 与 pyim-probe-dynamic-english 配合，方便切换至中文模式
  :bind
  (
   ("<f2> SPC" . pyim-convert-string-at-point)
   ("M-SPC" . toggle-input-method)
   (:map pyim-mode-map
    ;; ("<f2> TAB" . pyim-toggle-assistant-scheme)
    ("/" . pyim-toggle-assistant-scheme)
    ("_" . pyim-toggle-input-ascii)
    ("\\" . pyim-toggle-input-ascii)
    )))
;; input method:1 ends here

;; [[file:../../doom.note::*theme][theme:1]]
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-acario-light)
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-material)
(setq doom-theme 'doom-nova)
;; (setq doom-theme 'doom-vibrant)
;; (load-theme 'material t)
(custom-set-faces!
  '(org-todo :foreground "yellow" :weight bold :background "#263238")
  '(org-done :foreground "yellow" :weight bold :background "#263238")
  '(org-table :foreground "#e3f2fd")
  '(hl-line :background "#37474f")
  '(org-level-1 :foreground "#e3f2fd" :height 1.1 :background nil :weight bold :box nil)
  '(org-level-2 :foreground "#e3f2fd" :height 1.0 :background nil :weight normal :box nil)
  '(org-headline-done :foreground "gray" :weight normal)
  )

;; 当前行
(custom-set-faces!
  '(hl-line :background "#37474f")
  '(solaire-hl-line-face :background "#37474f")
  '(region :background "#555555")
  )
;; theme:1 ends here

;; [[file:../../doom.note::*修改 frame 标题 方便 gnome-shell 桌面切换][修改 frame 标题 方便 gnome-shell 桌面切换:1]]
;; workspace@buffer-name: ~/foo/bar
(setq frame-title-format
      '((:eval (+workspace-current-name)) ;
        " | %b : "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))))
        ))
;; 修改 frame 标题 方便 gnome-shell 桌面切换:1 ends here

;; [[file:../../doom.note::*放大当前窗口][放大当前窗口:1]]
(use-package! golden-ratio
  :config
  (map! :map evil-window-map
        "z" #'golden-ratio))
;; 放大当前窗口:1 ends here

;; [[file:../../doom.note::*window切换][window切换:1]]
(use-package! avy
              :config
              (setq avy-all-windows t)
              )
;; 替代 SPC-w-w
(global-set-key [remap evil-window-next] #'ace-window)
;; window切换:1 ends here

;; [[file:../../doom.note::*弹出窗口管理][弹出窗口管理:1]]
(map! :i "C-`" #'+popup/toggle)
;; 弹出窗口管理:1 ends here

;; [[file:../../doom.note::*窗口大小调整][窗口大小调整:1]]
(map! :nvi
      [C-M-mouse-4] #'evil-window-increase-width
      [C-M-mouse-5] #'evil-window-decrease-width
      )
;; 窗口大小调整:1 ends here

;; [[file:../../doom.note::*窗口大小调整][窗口大小调整:2]]
(setq split-width-threshold 200)        ; default is 160
;; 窗口大小调整:2 ends here

;; [[file:../../doom.note::*窗口大小调整][窗口大小调整:3]]
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; 窗口大小调整:3 ends here

;; [[file:../../doom.note::*line number][line number:1]]
(setq display-line-numbers-type nil)
;; line number:1 ends here

;; [[file:../../doom.note::*misc][misc:1]]
;; View images inside Emacs
(auto-image-file-mode t)

;; set line space wider than default
(setq-default line-spacing 4)
;; misc:1 ends here

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
        )
      )

    (map! :map dired-mode-map
          :localleader
          :n "y" #'gwp/dired-copy-file-path
          :n "l" #'dired-do-symlink
          )

    ;; 使用BACKSPACE来上一级目录, 使用Ctrl-shift-n来新建目录(默认为"+")
    (map! :map dired-mode-map
          :nv "DEL"   #'dired-up-directory       ; BACKSPACE
          :nv "C-S-n" #'dired-create-directory
          ;; :nv [mouse-1] #'dired-find-file
          )
    )
  )
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

;; treat .note files as org-mode
(add-to-list 'auto-mode-alist '("\\.note\\'" . org-mode))
(add-to-list 'auto-mode-alist '("NOTE" . org-mode))

(after! org (load! "org"))

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

;; [[file:../../doom.note::*ripgrep][ripgrep:1]]
;;;###autoload
(defun gwp/search-all-notes (arg)
  "search all notes in ~/.cache/notes"
  (interactive "P")

  (let ((default-directory "~/.cache/notes"))
    (call-interactively '+ivy/project-search-from-cwd)
    )
  )
;; ripgrep:1 ends here

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

;; [[file:../../doom.note::*bindings][bindings:1]]
(map! :nvim "C-a" nil)
;; 禁用evil中的ctrl-e, 默认为向上滚动, 不太习惯.
(map! :nvim "C-e" nil)
(map! :nvim "C-d" nil)
(map! :nvim "C-k" nil)
(map! :nvim "C-n" nil)
(map! :nvim "C-p" nil)
(map! :nvim "C-u" nil)                  ; universal argument

(map! :vi "C-w" #'kill-region)          ; cut, copy: Alt-w

;; evil默认为quoted-insert, 可以 ctrl-q代替
(map! :i "C-v" #'yank)
(map! :i "C-y" nil)

;; evil里也得设置, 不然无效
(after! evil-org
        (map! :map evil-org-mode-map
              :nivm "C-d" nil
              :nivm "C-k" nil
              :i "M-l" nil
              )
        )
;; bindings:1 ends here

;; [[file:../../doom.note::*bindings][bindings:2]]
;; Make M-x harder to miss
(define-key! 'override
  "M-x" #'execute-extended-command
  "A-x" #'execute-extended-command)

;; A Doom convention where C-s on popups and interactive searches will invoke
;; ivy/helm for their superior filtering.
(define-key! :keymaps +default-minibuffer-maps
  "C-s" (if (featurep! :completion ivy)
            #'counsel-minibuffer-history
          #'helm-minibuffer-history))

;; Smarter C-a/C-e for both Emacs and Evil. C-a will jump to indentation.
;; Pressing it again will send you to the true bol. Same goes for C-e, except
;; it will ignore comments+trailing whitespace before jumping to eol.
(map! :gi "C-a" #'doom/backward-to-bol-or-indent
      :gi "C-e" #'doom/forward-to-last-non-comment-or-eol
      ;; Standardizes the behavior of modified RET to match the behavior of
      ;; other editors, particularly Atom, textedit, textmate, and vscode, in
      ;; which ctrl+RET will add a new "item" below the current one
      :gn [C-return]    #'+default/newline-below
      :gn [C-S-return]  #'+default/newline-above
      )

(load! "bindings")
;; bindings:2 ends here

;; [[file:../../doom.note::*dired-sidebar][dired-sidebar:1]]
(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar))
;; dired-sidebar:1 ends here

;; [[file:../../doom.note::*before save][before save:1]]
;; (remove-hook 'before-save-hook 'ws-butler-before-save)
;; before save:1 ends here
