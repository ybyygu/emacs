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
     ;; '("j" "C-n")
     ;; '("k" "C-p")
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

;; applies to all layouts (except dvp)
(defun meow/setup ()
  (map! :map meow-normal-state-keymap
        "0" #'meow-expand-0
        "1" #'meow-expand-1
        "2" #'meow-expand-2
        "3" #'meow-expand-3
        "4" #'meow-expand-4
        "5" #'meow-expand-5
        "6" #'meow-expand-6
        "7" #'meow-expand-7
        "8" #'meow-expand-8
        "9" #'meow-expand-9
        "-" #'negative-argument
        ;; ";" #'meow-reverse
        ;; "," #'meow-inner-of-thing
        ;; "." #'meow-bounds-of-thing
        ;; "'" #'repeat
        "<escape>" #'ignore))

;; Qwerty
(defun meow/setup-qwerty ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow/setup)
  (when (featurep! :gwp core +override)
    (meow/setup-motion))
  (map! :map meow-normal-state-keymap
        "h" #'meow-left
        "j" #'meow-next
        "k" #'meow-prev
        "l" #'meow-right
        "i" #'meow-insert
        "x" #'meow-line
        "d" #'meow-delete
        "c" #'meow-change
        "a" #'meow-append
        "y" #'meow-save
        "p" #'meow-yank
        "u" #'gwp::undo-dwim
        ;; "[" #'meow-beginning-of-thing
        ;; "]" #'meow-end-of-thing
        ;; "A" #'meow-open-below
        ;; "b" #'meow-back-word
        ;; "B" #'meow-back-symbol
        ;; "D" #'meow-backward-delete
        ;; "e" #'meow-next-word
        ;; "E" #'meow-next-symbol
        ;; "f" #'meow-find
        ;; "g" #'meow-cancel-selection
        ;; "G" #'meow-grab
        ;; "H" #'meow-left-expand
        ;; "I" #'meow-open-above
        ;; "J" #'meow-next-expand
        ;; "K" #'meow-prev-expand
        ;; "L" #'meow-right-expand
        ;; "m" #'meow-join
        ;; "n" #'meow-search
        ;; "o" #'meow-block
        ;; "O" #'meow-to-block
        ;; "q" #'meow-quit
        ;; "Q" #'meow-goto-line
        ;; "r" #'meow-replace
        ;; "R" #'meow-swap-grab
        ;; "s" #'meow-kill
        ;; "t" #'meow-till
        ;; "v" #'meow-visit
        ;; "w" #'meow-mark-word
        ;; "W" #'meow-mark-symbol
        ;; "X" #'meow-goto-line
        ;; "Y" #'meow-sync-grab
        ;; "z" #'meow-pop-selection
        ))

(use-package! meow
  :hook (doom-init-modules . meow-global-mode)
  :demand t
  :config
  (meow/setup-qwerty)
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
