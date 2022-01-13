;; [[file:../../../gwp.note::9f41280c][9f41280c]]
(defun gwp::undo-dwim ()
  (interactive)
  (if (region-active-p)
      (let ((current-prefix-arg '(4)))     ; C-u
        (call-interactively #'undo))
    (call-interactively #'undo)))

;; 默认evil的undo会将多个小操作合并为一个大的, undo时很不适应.
;; (setq evil-want-fine-undo t)
;; 9f41280c ends here

;; [[file:../../../gwp.note::e08c1132][e08c1132]]
;;; editor/core/config.el -*- lexical-binding: t; -*-

;; Leader Key
(defun meow/setup-leader ()
  ;; 问题: meow 中的 keypad 与 which-key 配合不好
  (meow-normal-define-key
   ;; local leader keys
   ;; `("w" . ,gwp::dwim-leader-key)
   ;; '("g" . "H-g")
   ;; `("g" . ,gwp::goto-leader-key)
   ;; '("," . "H-,")
   ;; `("," . ,gwp::local-leader-key)
   )

  ;; 与 which-key 配合更好
  ;; FIXME: using predefined vars
  (map! :map meow-normal-state-keymap
        "," (general-simulate-key "s-,")
        "g" (general-simulate-key "s-g")
        "w" (general-simulate-key "s-w")
        )

  (map! :map meow-motion-state-keymap
        "," (general-simulate-key "s-,")
        "g" (general-simulate-key "s-g")
        "w" (general-simulate-key "s-w")
        )

  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   )

  (map! :leader
        "?" #'meow-cheatsheet
        "/" #'meow-keypad-describe-key
        "1" #'meow-digit-argument
        "2" #'meow-digit-argument
        "3" #'meow-digit-argument
        "4" #'meow-digit-argument
        "5" #'meow-digit-argument
        "6" #'meow-digit-argument
        "7" #'meow-digit-argument
        "8" #'meow-digit-argument
        "9" #'meow-digit-argument
        "0" #'meow-digit-argument))

;; Keypad
(defun meow/setup-keypad ()
  (map! :map meow-leader-keymap
        "?" #'meow-cheatsheet
        "/" #'meow-keypad-describe-key
        "1" #'meow-digit-argument
        "2" #'meow-digit-argument
        "3" #'meow-digit-argument
        "4" #'meow-digit-argument
        "5" #'meow-digit-argument
        "6" #'meow-digit-argument
        "7" #'meow-digit-argument
        "8" #'meow-digit-argument
        "9" #'meow-digit-argument
        "0" #'meow-digit-argument
        "h" #'help-command))

;; 比如 dired, magit 生成的 buffer, 也许单独处理更好?
(defun meow/setup-motion ()
  (meow-motion-overwrite-define-key
   '("j" "meow-next")
   '("k" "meow-prev")
   )
  (when (featurep! :editor meow +leader)
    (meow-motion-overwrite-define-key
     '("\\ j" "H-j")
     '("\\ k" "H-k")))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" "H-j")
   '("k" "H-k"))
  )

;; Qwerty, normal state
(defun meow/setup-normal ()
  ;; normal commands
  (meow-normal-define-key
   '("<escape>" . keyboard-quit)
   '("1" . meow-1)
   '("2" . meow-2)
   '("3" . meow-3)
   '("4" . meow-4)
   '("5" . meow-5)
   '("6" . meow-6)
   '("7" . meow-7)
   '("8" . meow-8)
   '("9" . meow-9)
   '("0" . meow-0)
   '("-" . negative-argument)
   '("h" . meow-left)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   '("i" . meow-insert)
   '("x" . meow-line)
   '("d" . meow-delete)
   '("c" . meow-change)
   '("a" . meow-append)
   '("y" . meow-save)
   '("p" . meow-yank)
   '("u" . gwp::undo-dwim)
   ))

(use-package! meow
  :hook (doom-init-modules . meow-global-mode)
  :demand t
  :custom
  (setq meow-expand-hint-remove-delay 3.0)
  :config
  (meow/setup-normal)
  (cond
   ((featurep! :gwp core +leader)
    (map! :map meow-normal-state-keymap
          doom-leader-key doom-leader-map)
    (map! :map meow-motion-state-keymap
          doom-leader-key doom-leader-map)
    (map! :map meow-beacon-state-keymap
          doom-leader-key nil)
    (meow/setup-leader))
   (t (meow/setup-keypad)))
  (map! :map meow-keymap [remap describe-key] #'helpful-key))
;; e08c1132 ends here
