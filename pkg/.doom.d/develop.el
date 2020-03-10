;; [[file:~/Workspace/Programming/emacs/doom.note::*magit][magit:1]]
(after! magit
  ;;禁用magit中的gravatars支持, 响应能快一些.
  (setq magit-revision-show-gravatars nil)

  (map! :map doom-leader-git-map "s" #'magit-status)
  )
;; magit:1 ends here

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

;; [[file:~/Workspace/Programming/emacs/doom.note::*TODO rust][rust:1]]
(after! smartparens
        (sp-local-pair 'rust-mode "{" nil :post-handlers '(:add ("||\n[i]" "RET"))))
;; rust:1 ends here
