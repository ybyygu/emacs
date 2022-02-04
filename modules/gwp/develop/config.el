;; [[file:../../../gwp.note::8f612833][8f612833]]
(require 'init-magit)

(gwp::local-leader-def
  :keymaps 'magit-mode-map
  "D" 'magit-file-delete
  "j" 'magit-dired-jump
  "o" 'magit-diff-visit-file-other-window
  "O" 'magit-diff-visit-file-other-frame
  "r" 'magit-file-rename
  "t" 'magit-todos-list
  "f" 'magit-find-file
  )
;; 8f612833 ends here

;; [[file:../../../gwp.note::d28bc89a][d28bc89a]]
;; Use hippie-expand instead of dabbrev-expand
;; (global-set-key (kbd "M-/") #'dabbrev-expand)
(global-set-key (kbd "M-/") #'hippie-expand)
;; the same behavior as the original `dabbrev-expand'
(setq hippie-expand-dabbrev-skip-space t)

;; adjust the list of functions that hippie-expand will try
(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev-visible      ; first try the expansions from the currently visible parts
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-line
        try-expand-dabbrev-from-kill
        ;; try-expand-all-abbrevs
        ;; try-expand-list
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        ))

(after! company
  (setq company-idle-delay 1.5
        company-minimum-prefix-length 2))

(use-package! citre
  :defer t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  (map! :leader
        (:prefix-map ("j" . "jump")
         (:prefix-map ("c" . "citre")
          :desc "citre jump to definition"                    "d" #'citre-jump
          :desc "citre jump back"                             "b" #'citre-jump-back
          :desc "citre peek"                                  "p" #'citre-peek
          ))))
;; d28bc89a ends here

;; [[file:../../../gwp.note::3ac9a958][3ac9a958]]
(use-package vc
  :init
  (add-hook 'vc-before-checkin-hook #'time-stamp))

(use-package vc-hooks
  :init
  ;; Don't ask if I want to visit a sym-linked file under VC. I always want to!
  (setq vc-follow-symlinks t))
;; 3ac9a958 ends here

;; [[file:../../../gwp.note::81cb1ab5][81cb1ab5]]
(use-package find-file-in-project
  :config
  (setq ffip-use-rust-fd t))

;;;###autoload
(defun gwp::find-file-from-clipboard ()
  "打开 clipboard 中复制的文件路径"
  (interactive)
  (require 'find-file-in-project)
  (let ((path (simpleclip-get-contents)))
    (ffip-find-files path nil)))
;; 81cb1ab5 ends here

;; [[file:../../../gwp.note::*edit][edit:1]]
(after! smartparens
  (sp-local-pair 'rust-mode "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
  ;; Rust closure中使用
  (sp-with-modes '(rust-mode)
    (sp-local-pair "|" "|"))
  )

(add-hook 'rust-mode-hook
          (lambda () (require 'smartparens-rust)))

(after! org-src
  (add-to-list 'org-src-lang-modes '("rust" . rust)))
;; edit:1 ends here

;; [[file:../../../gwp.note::*cargo/rust-mode][cargo/rust-mode:2]]
(require 'rust-mode)
(require 'cargo)

;; taken from rust-cargo.el
(defun gwp/cargo-compile (args)
  ;; taken from cargo.el
  ;;
  ;; workaround cargo issue: https://github.com/rust-lang/cargo/issues/5895
  ;;
  ;; to make "jump-to-error" work, we need start compilation in workspace root dir
  (let (
        ;; save current directory
        (old-directory default-directory)
        (default-directory (or (cargo-process--workspace-root)
                               default-directory))
        )
    (compile (format "cargo.sh \"%s\" %s" old-directory args))
    ))

(defun gwp/rust-cargo-tangle-watch-check ()
  "Compile using `cargo watch and check`
The org src will be tangled first before compiling.
"
  (interactive)
  (gwp/org-babel-tangle-dwim)
  (gwp/rust-cargo-watch-check))

(defun gwp/rust-cargo-watch-check ()
  "Compile using `cargo watch and check`
"
  (interactive)
  (gwp/cargo-compile "check -q"))

(defun gwp/rust-cargo-tangle-watch-test ()
  "Compile using `cargo watch and test`
The org src will be tangled first before compiling.
"
  (interactive)
  (gwp/org-babel-tangle-dwim)
  (gwp/rust-cargo-watch-test))

(defun gwp/rust-cargo-watch-test ()
  "Compile using `cargo watch and test`
The org src will be tangled first before compiling.
"
  (interactive)
  (gwp/cargo-compile "d"))

(defun gwp/rust-cargo-update ()
  "Execute `cargo update` command"
  (interactive)
  (gwp/cargo-compile "update"))

(defun gwp/rust-cargo-doc-open ()
  "Execute `cargo doc --open` command"
  (interactive)
  (gwp/cargo-compile "doc --open --no-deps"))

(defun gwp/rust-cargo-edit-upgrade ()
  "Execute `cargo upgrade --dry-run` command"
  (interactive)
  (gwp/cargo-compile "upgrade --dry-run"))

;; ;; 修改popup window, 放大一些, 方便查看.
;; (set-popup-rule! "^\\*compilation\\*" :size 0.85 :quit t :select t :ttl nil)
;; gwp/org-babel-tangle-dwim

(require 'transient)
(transient-define-prefix gwp/rust-cargo-transient ()
  "rust development tools"
  [["compile org src:"
    :if org-in-src-block-p
    ("o b" "tangle src only" gwp/org-babel-tangle-dwim)
    ("o c" "tangle src & cargo check" gwp/rust-cargo-tangle-watch-check)
    ("o t" "tangle src & cargo test" gwp/rust-cargo-tangle-watch-test)
    ]]
  [["compile rust project:"
    ("c" "cargo check" gwp/rust-cargo-watch-check)
    ("t" "cargo test" gwp/rust-cargo-watch-test)
    ("d" "cargo doc" gwp/rust-cargo-doc-open)
    ("u" "cargo update" gwp/rust-cargo-update)
    ("U" "cargo upgrade" gwp/rust-cargo-edit-upgrade)
    ]]
  )

(map! :map org-mode-map
      :localleader
      "1" #'gwp/rust-cargo-transient
      )
(map! :map rust-mode-map
      :localleader
      "1" #'gwp/rust-cargo-transient
      )
;; cargo/rust-mode:2 ends here

;; [[file:../../../gwp.note::*racer][racer:1]]
(use-package cargo
  :defer t
  :init
  (progn
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    ))
;; racer:1 ends here

;; [[file:../../../gwp.note::151a16d0][151a16d0]]
(map! :map rust-mode-map
      "C-c C-f" #'rust-format-buffer
      )

;; 2022-01-14: 无效
;; (use-package rust-mode
;;   :hook (rust-mode . gwp/override-doom-format-buffer)
;;   )

;; (defun gwp/override-doom-format-buffer ()
;;   (local-set-key (kbd "SPC c f") 'rust-format-buffer))

(gwp::dwim-leader-def
  :keymaps 'rust-mode-map
  "f" 'rust-format-buffer
  )
;; 151a16d0 ends here

;; [[file:../../../gwp.note::72f0d377][72f0d377]]
(defun gwp/rust-insert-option (&optional result)
  "Insert the Option type."
  (interactive)

  (when (region-active-p)
    (sp-wrap-with-pair "<")
    (backward-char)
    (insert (or result "Option"))
    ))

(defun gwp/rust-unwrap-option (&optional result)
  "Remove Option type wrapper"
  (interactive)

  (when (region-active-p)
    (when (> (mark) (point))
      (exchange-point-and-mark))
    (sp-unwrap-sexp)
    (when (search-backward (or result "Option") (line-beginning-position) t)
      (delete-region (match-beginning 0) (match-end 0))
      )))


;; https://github.com/Wilfred/.emacs.d/blob/gh-pages/user-lisp/rust-customisations.el
(defun gwp/rust-toggle-pub ()
  "Toggle the public visibility of the function at point."
  (interactive)
  (save-excursion
    ;; If we're already at the beginning of the function definition,
    ;; `beginning-of-defun' moves to the previous function, so move elsewhere.
    (end-of-line)
    (beginning-of-defun)
    (if (looking-at "pub ")
        (delete-char 4)
      (insert "pub "))))

(defun gwp/rust-insert-result ()
  (interactive)
  (gwp/rust-insert-option "Result"))

(defun gwp/rust-unwrap-result ()
  (interactive)
  (gwp/rust-unwrap-option "Result"))

(transient-define-prefix gwp/rust-edit-transient ()
  "rust development tools"
  [["Result"
    ("o" "Wrap in Option" gwp/rust-insert-option)
    ("u" "Unwrap Option" gwp/rust-unwrap-option)
    ]]
  [["Result"
    ("r" "Wrap in Result" gwp/rust-insert-result)
    ("k" "Unwrap Result" gwp/rust-unwrap-result)
    ]]
  [["Pub"
    ("p" "toggle pub at point" gwp/rust-toggle-pub)
    ]]
  )

;; (defhydra gwp/rust-edit-hydra ()
;;   ("o" gwp/rust-insert-option "wrap with Option")
;;   ("u" gwp/rust-unwrap-option "unwrap Option")
;;   ("q" nil "quit")
;;   )

(map! :map rust-mode-map
      :localleader
      "e" #'gwp/rust-edit-transient
      :desc "select inner type" "m" (general-simulate-key "vi<")
      )
;; 72f0d377 ends here

;; [[file:../../../gwp.note::44b70ed9][44b70ed9]]
(defun gwp/tmux-ipython-paste-region (beg end &optional region)
  "Execute \"%paste\" in tmux session"
  (interactive "r")
  (kill-ring-save beg end)
  (+tmux/run "%paste"))
;; 44b70ed9 ends here

;; [[file:../../../gwp.note::985a2495][985a2495]]
(gwp::local-leader-def
  :keymaps 'emacs-lisp-mode-map
  "e" #'eval-last-sexp
  "r" #'eval-region
  "b" #'eval-buffer
  "d" #'eval-defun
  )
;; 985a2495 ends here

;; [[file:../../../gwp.note::13061ec7][13061ec7]]
(setq fortran-continuation-string "&")
(setq fortran-do-indent 2)
(setq fortran-if-indent 2)
(setq fortran-structure-indent 2)

;; Fortran 90 settings
(setq f90-do-indent 2)
(setq f90-if-indent 2)
(setq f90-type-indent 2)
(setq f90-program-indent 2)
(setq f90-continuation-indent 4)
(setq f90-smart-end 'blink)

;; Set Fortran and Fortran 90 mode for appropriate extensions
(setq auto-mode-alist
      (cons '("\\.F90$" . f90-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.pf$" . f90-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.fpp$" . f90-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.F$" . f90-mode) auto-mode-alist))

;; Swap Return and C-j in Fortran 90 mode
(add-hook 'f90-mode-hook
	  '(lambda ()
	     (define-key f90-mode-map [return] 'f90-indent-new-line)
	     (define-key f90-mode-map "\C-j" 'newline)
	     (setq fill-column 180)
             (abbrev-mode)
             (setq-default indent-tabs-mode nil)
             (setq whitespace-line-column 180)
             (setq whitespace-style '(face tabs lines-tail empty))
             (whitespace-mode)
             ;;         (add-to-list 'write-file-functions 'delete-trailing-whitespace)
	     ))
;; 13061ec7 ends here

;; [[file:../../../gwp.note::87bcf67b][87bcf67b]]
(defhydra gwp::hl-todo ()
  "highlight todo"
  ("o" hl-todo-occur "occur")
  ("j" hl-todo-next "next")
  ("k" hl-todo-previous "prev")
  ("q" nil "quit"))

(map! :leader
      (:prefix-map ("j" . "jump")
       :desc "highlight todo" "t" #'gwp::hl-todo/body
       ))

;; org-mode 中默认不开 hl-todo
;; (add-hook! (org-mode) :append #'hl-todo-mode)

(gwp::local-leader-def
  :keymaps 'hl-todo-mode-map
  "H" '(gwp::hl-todo/body :which-key "highlight todo")
  )
;; 87bcf67b ends here

;; [[file:../../../gwp.note::d95b49c5][d95b49c5]]
(use-package block-nav
  :custom
  (block-nav-skip-comment t)
  (block-nav-move-skip-shallower  t)
  :config
  (map! :n "M-n" #'block-nav-next-block)
  (map! :n "M-p" #'block-nav-previous-block)
  (map! :n "M-l" #'block-nav-next-indentation-level)
  (map! :n "M-h" #'block-nav-previous-indentation-level)
  )
;; d95b49c5 ends here

;; [[file:../../../gwp.note::f28734ed][f28734ed]]
;; https://emacs.stackexchange.com/a/33747
(defun gwp::imenu-goto--closest-dir (direction)
  "Jump to the closest imenu item on the current buffer.
If direction is 1, jump to next imenu item.
If direction is -1, jump to previous imenu item.
See https://emacs.stackexchange.com/questions/30673
Adapted from `which-function' in::
https://github.com/typester/emacs/blob/master/lisp/progmodes/which-func.el"
  ;; Ensure `imenu--index-alist' is populated.
  (imenu--make-index-alist)

  (let ((alist imenu--index-alist)
        (minoffset (point-max))
        offset pair mark imstack destination)
    ;; Elements of alist are either ("name" . marker), or
    ;; ("submenu" ("name" . marker) ... ). The list can be
    ;; Arbitrarily nested.
    (while (or alist imstack)
      (if alist
          (progn
            (setq pair (car-safe alist)
                  alist (cdr-safe alist))
            (cond
             ((atom pair)) ;; Skip anything not a cons.

             ((imenu--subalist-p pair)
              (setq imstack   (cons alist imstack)
                    alist     (cdr pair)))

             ((number-or-marker-p (setq mark (cdr pair)))
              (when (> (setq offset (* (- mark (point)) direction)) 0)
                (when (< offset minoffset) ;; Find the closest item.
                  (setq minoffset offset
                        destination mark))))))

        (setq alist   (car imstack)
              imstack (cdr imstack))))
    (when destination
      (imenu-default-goto-function "" destination ""))))

(defun gwp::imenu-goto-next ()
  (interactive)
  (unless (gwp::imenu-goto--closest-dir 1)
    (goto-char (point-max)))
  (recenter))

(defun gwp::imenu-goto-prev ()
  (interactive)
  (unless (gwp::imenu-goto--closest-dir -1)
    (goto-char (point-min)))
  (recenter))

(map! :map prog-mode-map
      "M-n" #'gwp::imenu-goto-next
      "M-p" #'gwp::imenu-goto-prev
 )
;; f28734ed ends here
