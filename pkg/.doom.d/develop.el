;; [[file:~/Workspace/Programming/emacs/doom.note::*magit][magit:1]]
(after! magit
  ;;禁用magit中的gravatars支持, 响应能快一些.
  (setq magit-revision-show-gravatars nil)

  (map! :map doom-leader-git-map "s" #'magit-status)
  )
;; magit:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*cargo][cargo:1]]
(use-package cargo
  :defer t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode)      ; when edit Rust source
  (add-hook 'conf-toml-mode-hook 'cargo-minor-mode) ; when edit Cargo.toml
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
        (sp-local-pair 'rust-mode "{" nil :post-handlers '(:add ("||\n[i]" "RET"))))

;; 启用rust LSP: 用不起来
;; (after! rustic
;;   )
;; (setq rustic-lsp-server 'rust-analyzer)
;; edit:1 ends here
