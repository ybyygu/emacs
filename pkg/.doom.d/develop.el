;; [[file:~/Workspace/Programming/emacs/doom.note::*magit][magit:1]]
(after! magit
  ;;禁用magit中的gravatars支持, 响应能快一些.
  (setq magit-revision-show-gravatars nil)

  (map! :map doom-leader-git-map "s" #'magit-status)
  )
;; magit:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*cargo][cargo:1]]
(eval-when-compile (require 'el-patch))

(use-package cargo
  :defer t
  :hook (rust-mode . cargo-minor-mode)
  :config/el-patch
  (defun cargo-process--start (name command &optional last-cmd opens-external)
    "Start the Cargo process NAME with the cargo command COMMAND.
OPENS-EXTERNAL is non-nil if the COMMAND is expected to open an external application.
Returns the created process."
    (set-rust-backtrace command)
    (let* ((buffer (concat "*Cargo " name "*"))
           (project-root (cargo-process--project-root))
           (cmd
            (or last-cmd
                (cargo-process--maybe-read-command
                 (cargo-process--augment-cmd-for-os opens-external
                                                    (mapconcat #'identity (list (shell-quote-argument cargo-process--custom-path-to-bin)
                                                                                command
                                                                                ;; (manifest-path-argument name)
                                                                                cargo-process--command-flags)
                                                               " ")))))
           ;; (default-directory (or project-root default-directory))
           )
      (save-some-buffers (not compilation-ask-about-save)
                         (lambda ()
                           (and project-root
                                buffer-file-name
                                (string-prefix-p project-root (file-truename buffer-file-name)))))
      (setq cargo-process-last-command (list name command cmd))
      ;; (let ((default-directory (or (cargo-process--workspace-root)
      ;;                              default-directory)))
      ;;   (compilation-start cmd 'cargo-process-mode (lambda(_) buffer)))
      (compilation-start cmd 'cargo-process-mode (lambda(_) buffer))
      (let ((process (get-buffer-process buffer)))
        (set-process-sentinel process 'cargo-process--finished-sentinel)
        process)))
  :init
  (add-hook 'conf-toml-mode-hook 'cargo-minor-mode) ; when edit Cargo.toml
  (setq cargo-process--command-test "d")

  (require 'cargo-process)
  (defun gwp/cargo-process-watch ()
    "Run the Cargo check command.
With the prefix argument, modify the command's invocation.
Cargo: Check compile the current project.
Requires cargo-check to be installed."
    (interactive)
    (cargo-process--start "Watch" "watch -x check -x d"))

  (map! :map cargo-minor-mode-map
        :localleader
        (:prefix ("c" . "cargo")
          :desc "cargo check"
          "c" #'cargo-process-check
          :desc "cargo test (all)"
          "t" #'cargo-process-test
          :desc "cargo test (current)"
          "T" #'cargo-process-current-test
          :desc "cargo run"
          "r" #'cargo-process-test
          :desc "repeat last cargo cmd"
          "." #'cargo-process-repeat
          :desc "cargo clippy"
          "l" #'cargo-process-clippy
          :desc "cargo update"
          "u" #'cargo-process-update
          :desc "cargo doc --open"
          "d" #'cargo-process-doc-open
          :desc "cargo watch"
          "w" #'gwp/cargo-process-watch
          )))
;; cargo:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*racer][racer:1]]
(use-package cargo
  :defer t
  :init
  (progn
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    ))
;; racer:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*vc commit][vc commit:1]]
(use-package vc
  :init
  (progn
    (add-hook 'vc-before-checkin-hook #'time-stamp)
    )
  )

(use-package vc-hooks
  :init
  (progn
    ;; Don't ask if I want to visit a sym-linked file under VC. I always want to!
    (setq vc-follow-symlinks t)
    )
  )
;; vc commit:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*yadm \[\[https://github.com/TheLocehiliosan/yadm/blob/master/yadm.md\]\[yadm/yadm.md at master · TheLocehiliosan/yadm\]\]][yadm [[https://github.com/TheLocehiliosan/yadm/blob/master/yadm.md][yadm/yadm.md at master · TheLocehiliosan/yadm]]:1]]
;; (magit-status  "/yadm::")
(after! tramp
  (add-to-list 'tramp-methods
             '("yadm"
               (tramp-login-program "yadm")
               (tramp-login-args (("enter")))
               (tramp-login-env (("SHELL") ("/bin/sh")))
               (tramp-remote-shell "/bin/sh")
               (tramp-remote-shell-args ("-c"))))
  )
;; yadm [[https://github.com/TheLocehiliosan/yadm/blob/master/yadm.md][yadm/yadm.md at master · TheLocehiliosan/yadm]]:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*edit][edit:1]]
(after! smartparens
  (sp-local-pair 'rust-mode "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
  ;; Rust closure中使用
  (sp-with-modes '(rust-mode)
    (sp-local-pair "|" "|"))
  )

(add-hook 'rust-mode-hook
          (lambda () (require 'smartparens-rust)))


;; 启用rust LSP: 用不起来
;; (after! rustic
;;   )
;; (setq rustic-lsp-server 'rust-analyzer)
;; edit:1 ends here
