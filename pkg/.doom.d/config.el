;; orign

;; [[file:~/Workspace/Programming/emacs/doom.note::*orign][orign:1]]
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
(setq doom-font (font-spec :family "monospace" :size 14))

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
;; orign:1 ends here

;; 常用按键
;; 禁用evil中的ctrl-e, 默认为向上滚动, 不太习惯.

;; [[file:~/Workspace/Programming/emacs/doom.note::*常用按键][常用按键:1]]
(map! :map evil-motion-state-map "C-e" nil)
;; 常用按键:1 ends here

;; chinese fonts setup

;; [[file:~/Workspace/Programming/emacs/doom.note::*chinese fonts setup][chinese fonts setup:1]]
(use-package! cnfonts
  :config
  (progn
    (setq cnfonts-profiles
          '("program" "org-mode" "read-book"))
    (setq cnfonts-use-face-font-rescale t)
    )
  (cnfonts-enable)
  )
;; chinese fonts setup:1 ends here

;; theme

;; [[file:~/Workspace/Programming/emacs/doom.note::*theme][theme:1]]
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-acario-light)
;; theme:1 ends here

;; 基本设置

;; [[file:~/Workspace/Programming/emacs/doom.note::*基本设置][基本设置:1]]
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes/")

;; treat .note files as org-mode
(add-to-list 'auto-mode-alist '("\\.note\\'" . org-mode))
(add-to-list 'auto-mode-alist '("NOTE" . org-mode))

(after! org
  (setq org-blank-before-new-entry nil)
  (setq org-default-notes-file (concat org-directory "/life.note"))

  ;; 经常按错这个键, 禁用之
  (put 'org-toggle-comment 'disabled t)

  ;; 保留以前的 Alt-Return 键行为, Alt-Return
  (org-defkey org-mode-map [(meta return)] 'org-meta-return)

  ;; doom 默认 src 中不保留缩进.
  (setq org-src-preserve-indentation nil)
  )
;; 基本设置:1 ends here

;; 按键

;; [[file:~/Workspace/Programming/emacs/doom.note::*按键][按键:1]]
(after! org
  (map! :map org-mode-map
        :leader
        :desc "insert inactive timestamp"
        "m SPC !"
        #'org-time-stamp-inactive)
  )
;; 按键:1 ends here

;; 按 SPC-m SPC-t tangle当前代码:

;; [[file:~/Workspace/Programming/emacs/doom.note::*按 SPC-m SPC-t tangle当前代码:][按 SPC-m SPC-t tangle当前代码::1]]
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
;; 按 SPC-m SPC-t tangle当前代码::1 ends here
