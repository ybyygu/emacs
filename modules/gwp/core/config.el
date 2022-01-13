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
;;;###autoload
(defun gwp::meow-insert-at-the-end ()
  (interactive)
  (meow-line 1)
  (meow-append))

;;;###autoload
(defun gwp::meow-insert-at-the-beginning ()
  (interactive)
  (meow-join 1)
  (meow-append))

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
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("0" . meow-expand-0)
   '("-" . negative-argument)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("i" . meow-insert)
   '("I" . gwp::meow-insert-at-the-beginning)
   '("A" . gwp::meow-insert-at-the-end)
   '("x" . meow-line)
   '("d" . meow-kill)
   '("D" . meow-kill-whole-line)
   '("c" . meow-change)
   '("a" . meow-append)
   '("y" . meow-save)
   '("p" . meow-yank)
   '("o" . meow-reverse)
   '("O" . meow-open-above)
   '("J" . crux-top-join-line)
   '("u" . gwp::undo-dwim)
   '("U" . meow-pop-grab)               ; fallback to meow-pop-selection
   '("/" . meow-visit)
   '("n" . meow-search)
   '("v" . meow-cancel-selection)       ; 仿 vi
   '("V" . meow-line-expand)
   '("f" . meow-find)                   ; 含搜索字符
   '("t" . meow-till)                   ; 不含搜索字符
   '("%" . meow-block)
   '("*" . meow-mark-word)
   '("s" . meow-inner-of-thing)
   '("S" . meow-bounds-of-thing)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '(";" . meow-cancel-selection)
   '("G" . meow-grab)
   )

  ;; 当无选区时执行的功能
  (setq
   meow-selection-command-fallback
   '(
     (meow-reverse . meow-open-below)
     (meow-kill . meow-C-d)
     (meow-change . meow-change-char)
     (meow-pop-selection . meow-pop-grab)
     (meow-beacon-change . meow-beacon-change-char)
     (meow-cancel-selection . meow-right-expand) ; 仿vi, 取消选择或扩选
     )))

(use-package! meow
  :hook (doom-init-modules . meow-global-mode)
  :demand t
  :custom
  (meow-expand-hint-remove-delay 5.0)
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
