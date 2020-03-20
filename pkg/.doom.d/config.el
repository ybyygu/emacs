;; [[file:~/Workspace/Programming/emacs/doom.note::*orig][orig:1]]
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

;; [[file:~/Workspace/Programming/emacs/doom.note::*window][window:2]]
(use-package! zoom
              ;; :after-call pre-command-hook
              :config
              (custom-set-variables
               '(zoom-size '(0.618 . 0.618)))
              (map! :map evil-window-map
                    "z" #'zoom)
              )
;; window:2 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*window][window:4]]
(map! :i "C-`" #'+popup/toggle)
;; window:4 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*line number][line number:1]]
(setq display-line-numbers-type nil)
;; line number:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*doom tuning][doom tuning:1]]
(setq evil-want-fine-undo t)
;; doom tuning:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*insert date][insert date:1]]
(defun gwp/insert-date (arg)
  "Insert date at point. With prefix argument, insert date and time."
  (interactive "P")
  (insert (format-time-string "%Y-%m-%d"))
  (when arg
    (insert (format-time-string " %H:%M"))
    )
  )

;; make it easier to update time-stamp
(map! :i "C-c i" #'gwp/insert-date)
;; insert date:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*white space][white space:1]]
(setq show-trailing-whitespace t)
(global-set-key (kbd "<f5> SPC") 'delete-trailing-whitespace)
;; make sure this always work
(global-set-key (kbd "C-x C-o") 'delete-blank-lines)
;; white space:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*TODO advanced selection][advanced selection:1]]
;; expand selection
;; http://xahlee.org/emacs/modernization_mark-word.html
;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun gwp/extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

;; (global-set-key (kbd "<f5> v") 'gwp/extend-selection)

(defun gwp/select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters are paired characters: ()[]<>«»“”‘’「」, including \"\"."
  (interactive)
  (let (b1 b2)
    (skip-chars-backward "^<>(“{[「«\"‘")
    (setq b1 (point))
    (skip-chars-forward "^<>)”}]」»\"’")
    (setq b2 (point))
    (set-mark b1)
    )
  )

(defun gwp/select-none-blank-text ()
  "Select none blank chars near the point in current line"
  (interactive)
  (let (b1 b2)
    (skip-chars-backward "^ \n")
    (setq b1 (point))
    (skip-chars-forward "^ \n")
    (setq b2 (point))
    (set-mark b1)
    )
  )

(defun gwp/select-word ()
  "Select none blank chars near the point in current line"
  (interactive)
  (let (b1 b2)
    (backward-word)
    (setq b1 (point))
    (forward-word)
    (setq b2 (point))
    (set-mark b1)
    )
  )

(defun gwp/select-line ()
  "Select current line"
  (interactive)
  (let (b1 b2)
    (move-beginning-of-line nil)
    (setq b1 (point))
    (move-end-of-line nil)
    (setq b2 (point))
    (set-mark b1)
    )
  )

;; (global-set-key (kbd "M-*") 'select-text-in-quote)
;; (global-set-key (kbd "M-6") 'select-line)
;; (global-set-key (kbd "M-4") 'select-word)
(global-set-key (kbd "M-5") 'gwp/select-none-blank-text)

;; https://github.com/magnars/expand-region.el
;; (require 'expand-region)
;; (global-set-key (kbd "M-4") 'er/expand-region)
;; advanced selection:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*dired][dired:1]]
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
;; dired:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*dired][dired:2]]
(use-package dired-x
  :config
  (progn
    (setq dired-omit-verbose t)
    ;; (add-hook 'dired-mode-hook #'dired-omit-mode)
    ;; default is "-al"
    ;; (setq dired-listing-switches "--group-directories-first -al -X")
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))
  )
;; dired:2 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*org][org:1]]
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes/")

;; treat .note files as org-mode
(add-to-list 'auto-mode-alist '("\\.note\\'" . org-mode))
(add-to-list 'auto-mode-alist '("NOTE" . org-mode))

(after! org (load! "org"))
;; org:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*open-file-externally][open-file-externally:1]]
(defun spacemacs/open-in-external-app (file-path)
  "Open `file-path' in external application."
  (let ((process-connection-type nil))
    (start-process "" nil "xdg-open" file-path)
    )
   )

(defun spacemacs/open-file-or-directory-in-external-app (arg)
  "Open current file in external application.
If the universal prefix argument is used then open the folder
containing the current file by the default explorer."
  (interactive "P")
  (if arg
      (spacemacs/open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (spacemacs/open-in-external-app file-path)
        (message "No file associated to this buffer.")))
    )
  )
;; open-file-externally:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*open in terminal][open in terminal:1]]
(defun gwp/open-in-gnome-terminal (the-directory)
  "Open `the-directory' in external gnome-terminal."
  (let ((process-connection-type nil))
    ;; (start-process "" nil "gnome-terminal" (concat "--working-directory=" the-directory))
    (start-process "" nil "alacritty" (concat "--working-directory=" the-directory) "-e" "tmux")
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

;; [[file:~/Workspace/Programming/emacs/doom.note::*ripgrep][ripgrep:1]]
;;;###autoload
(defun gwp/search-all-notes (arg)
  "search all notes in ~/.cache/notes"
  (interactive "P")

  (let ((default-directory "~/.cache/notes"))
    (call-interactively '+ivy/project-search-from-cwd)
    )
  )
;; ripgrep:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*develop][develop:1]]
(load! "develop")
;; develop:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*recent files][recent files:1]]
(require 'recentf)
;; the default is only 20
(setq recentf-max-saved-items 1000)
(add-to-list 'recentf-exclude "autosave$")
(add-to-list 'recentf-exclude "\.png$")
(add-to-list 'recentf-exclude "\.pdf$")
(add-to-list 'recentf-exclude "\.svg$")
(add-to-list 'recentf-exclude "\.odt$")
;; recent files:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*fcitx][fcitx:1]]
(use-package! fcitx
  :after evil
  :config
  (when (executable-find "fcitx-remote")
    ;; (fcitx-prefix-keys-add "M-m")
    ;; 有响应问题
    ;; (setq fcitx-use-dbus t)
    (fcitx-aggressive-setup)
    ))
;; fcitx:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*chinese fonts setup][chinese fonts setup:1]]
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
        (setq doom-font (font-spec :family "Monaco" :size 16))
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
(call-interactively 'gwp/set-fonts)

;; org-mode表格中文混排对齐
(after! org
  (custom-set-faces!
    `(org-table :family "Ubuntu Mono")
    )
  )
;; chinese fonts setup:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*theme][theme:1]]
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-acario-light)
;; (setq doom-theme 'doom-one)
(load-theme 'material t)
;; theme:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*window][window:1]]
(setq frame-title-format '("%b: " buffer-file-name))
;; window:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*window][window:3]]
(use-package! avy
              :config
              (setq avy-all-windows t)
              )
;; window:3 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*misc][misc:1]]
;; View images inside Emacs
(auto-image-file-mode t)

;; set line space wider than default
(setq-default line-spacing 4)
;; misc:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*smartparens][smartparens:1]]
(map! :leader
      :nvi
      "DEL" #'sp-backward-kill-sexp     ; BACKSPACE
      [deletechar] #'sp-kill-sexp       ; DELETE
      [right] #'sp-forward-slurp-sexp   ; Array right ==>
      [left]  #'sp-backward-slurp-sexp  ; Array left  <==
      "C-k"   #'sp-unwrap-sexp
      )
;; smartparens:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*expand region][expand region:1]]
(use-package expand-region :after evil :config
  (map! :leader :v "v"
        (function er/expand-region)))
;; expand region:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*bindings][bindings:1]]
(map! :nvim "C-a" nil)
;; 禁用evil中的ctrl-e, 默认为向上滚动, 不太习惯.
(map! :nvim "C-e" nil)
(map! :nvim "C-d" nil)
(map! :nvim "C-k" nil)
(map! :nvim "C-n" nil)
(map! :nvim "C-p" nil)
(map! :nvim "C-u" nil)                  ; universal argument

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

;; [[file:~/Workspace/Programming/emacs/doom.note::*bindings][bindings:2]]
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
