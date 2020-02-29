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

(after! org
        (setq org-src-window-setup 'split-window-below)
        (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)
        )

(after! smartparens
        (sp-local-pair 'rust-mode "{" nil :post-handlers '(:add ("||\n[i]" "RET"))))

;; (after! rust-mode
;;         (use-package clean-aindent-mode
;;           :config (clean-aindent-mode)
;;           )
;;         )

(map! :map evil-motion-state-map "C-e" nil)
(map! :map evil-insert-state-map "C-n" nil)
(map! :map evil-insert-state-map "C-p" nil)
(map! :map evil-normal-state-map "C-n" nil)
(map! :map evil-normal-state-map "C-p" nil)

(map! :leader
      (:desc "Dired jump"              "fj"  #'dired-jump)
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
  )

(after! org
        ;; 经常按错这个键, 禁用之 (Ctrl-c ;)
        (put 'org-toggle-comment 'disabled t)

        (map! :map org-mode-map
              :ni "C-k" #'org-kill-line
              :ni "C-c ;" #'org-edit-special
              :ni "C-c C-;" #'org-edit-special
              :localleader ";" #'org-edit-special
              :n "h" #'org-up-element
              )
        (setq org-src-fontify-natively nil)
        )

(after! org
  (map! :map org-mode-map
        :leader
        :desc "tangle src blocks at point"
        "m SPC t"
        #'gwp/org-babel-tangle-blocks)
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

(after! magit
        (map! :map doom-leader-git-map "s" #'magit-status)
        )
