;; [[file:../../doom.note::*magit][magit:1]]
(after! magit
  ;;禁用magit中的gravatars支持, 响应能快一些.
  (setq magit-revision-show-gravatars nil)

  (map! :map doom-leader-git-map "s" #'magit-status)
  )
;; magit:1 ends here

;; [[file:../../doom.note::*vc commit][vc commit:1]]
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

;; [[file:../../doom.note::*yadm \[\[https:/github.com/TheLocehiliosan/yadm/blob/master/yadm.md\]\[yadm/yadm.md at master · TheLocehiliosan/yadm\]\]][yadm [[https://github.com/TheLocehiliosan/yadm/blob/master/yadm.md][yadm/yadm.md at master · TheLocehiliosan/yadm]]:1]]
;; (magit-status  "/yadm::")
(after! tramp
  (add-to-list 'tramp-methods
             '("yadm"
               (tramp-login-program "yadm")
               (tramp-login-args (("enter")))
               (tramp-login-env (("SHELL") ("/bin/sh")))
               (tramp-remote-shell "/bin/sh")
               (tramp-remote-shell-args ("-c")))))
;; yadm [[https://github.com/TheLocehiliosan/yadm/blob/master/yadm.md][yadm/yadm.md at master · TheLocehiliosan/yadm]]:1 ends here

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

;; [[file:../../doom.note::*bindings][bindings:1]]
(map! :map rust-mode-map
      :localleader
      "f" #'rust-format-buffer
      "C-f" #'rust-format-buffer
      "=" #'rust-format-buffer)
;; bindings:1 ends here

;; [[file:../../doom.note::*python.el][python.el:2]]
(defun gwp/tmux-ipython-paste-region (beg end &optional region)
  "Execute \"%paste\" in tmux session"
  (interactive "r")
  (kill-ring-save beg end)
  (+tmux/run "%paste"))
;; python.el:2 ends here
