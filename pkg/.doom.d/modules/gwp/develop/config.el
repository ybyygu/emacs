;; [[file:../../../../../doom.note::f28734ed][f28734ed]]
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

;; vim里没有Alt修饰, M-x类绑定可以放心用
(map! :n "M-n" #'gwp::imenu-goto-next)
(map! :n "M-p" #'gwp::imenu-goto-prev)
;; f28734ed ends here

;; [[file:../../../../../doom.note::d28bc89a][d28bc89a]]
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

;; [[file:../../../../../doom.note::81cb1ab5][81cb1ab5]]
(use-package find-file-in-project
  :config
  (setq ffip-use-rust-fd t))
;; 81cb1ab5 ends here

;; [[file:../../../../../doom.note::4bae51e2][4bae51e2]]
(use-package magit
  :config
  ;; 隐藏untracked文件列表. 更多时候的操作是stage/commit
  (setq magit-section-initial-visibility-alist (quote ((untracked . hide))))
  ;;禁用magit中的gravatars支持, 响应能快一些.
  (setq magit-revision-show-gravatars nil))
;; 4bae51e2 ends here

;; [[file:../../../../../doom.note::3ac9a958][3ac9a958]]
(use-package vc
  :init
  (add-hook 'vc-before-checkin-hook #'time-stamp))

(use-package vc-hooks
  :init
  ;; Don't ask if I want to visit a sym-linked file under VC. I always want to!
  (setq vc-follow-symlinks t))
;; 3ac9a958 ends here

;; [[file:../../../../../doom.note::0717be82][0717be82]]
;; (magit-status  "/yadm::")
(after! tramp
  (add-to-list 'tramp-methods
             '("yadm"
               (tramp-login-program "yadm")
               (tramp-login-args (("enter")))
               (tramp-login-env (("SHELL") ("/bin/sh")))
               (tramp-remote-shell "/bin/sh")
               (tramp-remote-shell-args ("-c")))))
;; 0717be82 ends here

;; [[file:../../../../../doom.note::*edit][edit:1]]
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

;; [[file:../../../../../doom.note::*cargo/rust-mode][cargo/rust-mode:2]]
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

;; [[file:../../../../../doom.note::*racer][racer:1]]
(use-package cargo
  :defer t
  :init
  (progn
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    ))
;; racer:1 ends here

;; [[file:../../../../../doom.note::151a16d0][151a16d0]]
(map! :map rust-mode-map
      :localleader
      "f" #'rust-format-buffer
      "C-f" #'rust-format-buffer
      "=" #'rust-format-buffer)

(use-package rust-mode
  :hook (rust-mode . gwp/override-doom-format-buffer)
  )

(defun gwp/override-doom-format-buffer ()
  (evil-local-set-key 'normal (kbd "SPC =") 'rust-format-buffer)
  ;; 无效
  ;; (evil-local-set-key 'normal (kbd "SPC c f") 'rust-format-buffer)
  )
;; 151a16d0 ends here

;; [[file:../../../../../doom.note::72f0d377][72f0d377]]
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

;; [[file:../../../../../doom.note::44b70ed9][44b70ed9]]
(defun gwp/tmux-ipython-paste-region (beg end &optional region)
  "Execute \"%paste\" in tmux session"
  (interactive "r")
  (kill-ring-save beg end)
  (+tmux/run "%paste"))
;; 44b70ed9 ends here