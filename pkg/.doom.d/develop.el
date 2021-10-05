;; [[file:../../doom.note::4bae51e2][4bae51e2]]
(after! magit
  ;;禁用magit中的gravatars支持, 响应能快一些.
  (setq magit-revision-show-gravatars nil))
;; 4bae51e2 ends here

;; [[file:../../doom.note::3ac9a958][3ac9a958]]
(use-package vc
  :init
  (add-hook 'vc-before-checkin-hook #'time-stamp))

(use-package vc-hooks
  :init
  ;; Don't ask if I want to visit a sym-linked file under VC. I always want to!
  (setq vc-follow-symlinks t))
;; 3ac9a958 ends here

;; [[file:../../doom.note::0717be82][0717be82]]
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

;; [[file:../../doom.note::*edit][edit:1]]
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

;; [[file:../../doom.note::*cargo/rust-mode][cargo/rust-mode:2]]
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

;; [[file:../../doom.note::*racer][racer:1]]
(use-package cargo
  :defer t
  :init
  (progn
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    ))
;; racer:1 ends here

;; [[file:../../doom.note::151a16d0][151a16d0]]
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

;; [[file:../../doom.note::*python.el][python.el:2]]
(defun gwp/tmux-ipython-paste-region (beg end &optional region)
  "Execute \"%paste\" in tmux session"
  (interactive "r")
  (kill-ring-save beg end)
  (+tmux/run "%paste"))
;; python.el:2 ends here
