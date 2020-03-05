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
(setq doom-font (font-spec :family "Monaco" :size 14))

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

(setq evil-want-fine-undo t)

(use-package! zoom
              ;; :after-call pre-command-hook
              :config
              (custom-set-variables
               '(zoom-size '(0.618 . 0.618)))
              (map! :map evil-window-map
                    "z" #'zoom)
              )

(map! :i "C-`" #'+popup/toggle)

(setq display-line-numbers-type nil)

(defun gwp/dired-copy-file-path()
  (interactive)
  (let ((current-prefix-arg '(0)))
    (call-interactively 'dired-copy-filename-as-kill)
    )
  )

(map! :map dired-mode-map
      :localleader
      :n "y" #'gwp/dired-copy-file-path
      )

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
          (spacemacs//open-in-external-app file-path)
        (message "No file associated to this buffer.")))
    )
  )

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

;;;###autoload
(defun gwp/search-all-notes (arg)
  "search all notes in ~/.cache/notes"
  (interactive "P")

  (let ((default-directory "~/.cache/notes"))
    (call-interactively '+ivy/project-search-from-cwd)
    )
  )

(defun gwp/dwim-at-point ()
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- clock: update its time.
- headline: toggle latex fragments and inline images underneath.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: 改为编辑代码, edit-special
- statistics-cookie: update it.
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive)
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    ;; skip over unimportant contexts
    (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
      (setq context (org-element-property :parent context)
            type (org-element-type context)))
    (pcase type
      (`headline
       (cond ((and (fboundp 'toc-org-insert-toc)
                   (member "TOC" (org-get-tags)))
              (toc-org-insert-toc)
              (message "Updating table of contents"))
             ((string= "ARCHIVE" (car-safe (org-get-tags)))
              (org-force-cycle-archived))
             ((or (org-element-property :todo-type context)
                  (org-element-property :scheduled context))
              (org-todo
               (if (eq (org-element-property :todo-type context) 'done)
                   (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                       'todo)
                 'done)))
             (t
              (+org--refresh-inline-images-in-subtree)
              (org-clear-latex-preview)
              (org-latex-preview '(4)))))

      (`clock (org-clock-update-time-maybe))

      (`footnote-reference
       (org-footnote-goto-definition (org-element-property :label context)))

      (`footnote-definition
       (org-footnote-goto-previous-reference (org-element-property :label context)))

      ((or `planning `timestamp)
       (org-follow-timestamp-link))

      ((or `table `table-row)
       (if (org-at-TBLFM-p)
           (org-table-calc-current-TBLFM)
         (ignore-errors
           (save-excursion
             (goto-char (org-element-property :contents-begin context))
             (org-call-with-arg 'org-table-recalculate (or arg t))))))

      (`table-cell
       (org-table-blank-field)
       (org-table-recalculate)
       (when (and (string-empty-p (string-trim (org-table-get-field)))
                  (bound-and-true-p evil-local-mode))
         (evil-change-state 'insert)))

      (`babel-call
       (org-babel-lob-execute-maybe))

      (`statistics-cookie
       (save-excursion (org-update-statistics-cookies nil)))

      ((or `src-block `inline-src-block)
       ;; 还是挺方便的
       (org-edit-special))

      ((or `latex-fragment `latex-environment)
       (org-latex-preview))

      (`link
       (let* ((lineage (org-element-lineage context '(link) t))
              (path (org-element-property :path lineage)))
         (if (or (equal (org-element-property :type lineage) "img")
                 (and path (image-type-from-file-name path)))
             (+org--refresh-inline-images-in-subtree)
           (org-open-at-point))))

      ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
       (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
         (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

      (_ (+org--refresh-inline-images-in-subtree)))))

;; helper functions for literate programming
;; taking from: https://github.com/grettke/help/blob/master/Org-Mode_Fundamentals.org
(defun help/set-org-babel-default-header-args (property value)
  "Easily set system header arguments in org mode.

PROPERTY is the system-wide value that you would like to modify.

VALUE is the new value you wish to store.

Attribution: URL `http://orgmode.org/manual/System_002dwide-header-arguments.html#System_002dwide-header-arguments'"
  (setq org-babel-default-header-args
        (cons (cons property value)
              (assq-delete-all property org-babel-default-header-args))))

(after! org
        (help/set-org-babel-default-header-args :padline "yes")
        (help/set-org-babel-default-header-args :mkdirp "yes")
        (help/set-org-babel-default-header-args :comments "link")
        )

(after! org
        ;; 禁用代码着色, 影响速度
        (setq org-src-fontify-natively nil)

        ;; 编辑代码时在下方新开窗口
        ;;(setq org-src-window-setup 'split-window-below)
        (setq org-src-window-setup 'current-window)
        ;;(setq org-src-window-setup 'reorganize-frame)
        ;;(setq org-src-window-setup 'other-frame)
        )

;; 进入代码编辑模式, 改成容易按的
(map! :map org-mode-map
      :ni "C-c ;" #'org-edit-special
      :ni "C-c C-;" #'org-edit-special
      :localleader ";" #'org-edit-special
      )

(after! org
        ;; 用于激活 localleader
        (add-hook 'org-src-mode-hook #'evil-normalize-keymaps)

        ;; 默认的不太好按. 不能用C-c C-c, 容易与别的模块冲突.
        (map! :map org-src-mode-map
              "C-c ;"   #'org-edit-src-exit  ; 保存退出
              "C-c C-;" #'org-edit-src-exit  ; 保存退出
              "C-c C-k" #'org-edit-src-abort ; 放弃修改
              )
        (map! :map org-src-mode-map
              :localleader
              ";" #'org-edit-src-exit
              "c" #'org-edit-src-exit
              "k" #'org-edit-src-abort
              )
        (map! :map rust-mode-map
              :localleader
              "=" #'rust-format-buffer
              )
        )

(after! smartparens
        (sp-local-pair 'rust-mode "{" nil :post-handlers '(:add ("||\n[i]" "RET"))))

;; (after! rust-mode
;;         (use-package clean-aindent-mode
;;           :config (clean-aindent-mode)
;;           )
;;         )

(map! :nvim "C-a" nil)
;; 禁用evil中的ctrl-e, 默认为向上滚动, 不太习惯.
(map! :nvim "C-e" nil)
(map! :nvim "C-d" nil)
(map! :nvim "C-k" nil)
(map! :nvim "C-n" nil)
(map! :nvim "C-p" nil)

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

(use-package! cnfonts
  :config
  (progn
    (setq cnfonts-profiles
          '("program" "org-mode" "read-book"))
    (setq cnfonts-use-face-font-rescale t)
    )
  (cnfonts-enable)
  )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-acario-light)
;; (setq doom-theme 'doom-one)
(load-theme 'material t)

(use-package! avy
              :config
              (setq avy-all-windows t)
              )

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes/")

;; treat .note files as org-mode
(add-to-list 'auto-mode-alist '("\\.note\\'" . org-mode))
(add-to-list 'auto-mode-alist '("NOTE" . org-mode))

(after! org
  (setq org-blank-before-new-entry nil)
  (setq org-default-notes-file (concat org-directory "/life.note"))

  ;; 保留以前的 Alt-Return 键行为, Alt-Return
  (org-defkey org-mode-map [(meta return)] 'org-meta-return)

  ;; doom 默认 src 中不保留缩进.
  (setq org-src-preserve-indentation nil)

  ;; 禁用字词检查, 需要了再开
  (remove-hook! 'org-mode-hook #'flyspell-mode)
  (flyspell-mode 0)
  )

(defun gwp/new-memo (arg)
  "Insert a new org-mode memo entry under heading at point."

  (interactive "P")

  (call-interactively 'evil-open-below)
  (insert "** ")
  (call-interactively 'org-time-stamp-inactive)
  (insert " ")
  )

(after! org
        ;; 经常按错这个键, 禁用之 (Ctrl-c ;)
        (put 'org-toggle-comment 'disabled t)

        (map! :map org-mode-map
              :n "gh" #'org-up-element
              :n "gl" #'org-down-element ; doom中默认为: evil-lion-left
              :n "gk" #'org-previous-visible-heading
              :n "gj" #'org-next-visible-heading
              :n "M-l" #'org-metaright   ; doom中默认为 demote-subtree
              :n "M-h" #'org-metaleft    ; doom中默认为 promote-subtree
              )

        (map! :map org-mode-map
              :localleader
              ;; FIXME: 与doom/org定义有冲突
              (:prefix ("s" . "Subtree")
                       :desc "Demote" "l" #'org-demote-subtree
                       :desc "Promote" "h" #'org-promote-subtree
                       :desc "Archive" "A" #'org-archive-subtree
                       :desc "Narrow" "n" #'org-toggle-narrow-to-subtree
                       )
              (:prefix ("SPC" . "Special")
                       :desc "org-ctrl-c-star" "s" #'org-ctrl-c-star ; 方便盲按
                       :desc "Insert new memo entry" "m" #'gwp/new-memo ; 简化操作
                       )
              )
        (map! :map org-mode-map
              :localleader
              (:prefix ("g" . "Goto")
                       :desc "Goto the previous position"  "p" #'org-mark-ring-goto
                       :desc "Jump to org heading"  "j" #'counsel-org-goto
                       :desc "Goto named src block" "b" #'org-babel-goto-named-src-block
                       )
              )
        )

(after! org
  (map! :map org-mode-map
        :localleader
        (:prefix ("b" . "org-babel")
                 :desc "insert header argument" "i" #'org-babel-insert-header-arg
                 :desc "tangle blocks at point" "b" #'gwp/org-babel-tangle-blocks
                 :desc "tangle blocks in subtree" "t" #'gwp/org-tangle-subtree
                 :desc "tangle blocks in buffer" "T" #'org-babel-tangle
                 )
        ;; 为了顺应spacemacs中的设置, 保留spc-ob 按键
        :leader
        :desc "tangle blocks at point" "o b" #'gwp/org-babel-tangle-blocks
        )
  )

;; tangle blocks for current file at point
;; http://stackoverflow.com/questions/28727190/org-babel-tangle-only-one-code-block
;; call org-babel-tangle with C-u C-u
(defun gwp/org-babel-tangle-blocks()
  (interactive)
  (let ((current-prefix-arg '(16)))
    (call-interactively 'org-babel-tangle)
    )
  )

;; narrow to subtree before calling org-babel-tangle
(defun gwp/org-tangle-subtree ()
  "tange src blocks in current subtree"
  (interactive)
  (org-narrow-to-subtree)
  (org-babel-tangle)
  (widen)
  )

(after! magit
        (map! :map doom-leader-git-map "s" #'magit-status)
        )
