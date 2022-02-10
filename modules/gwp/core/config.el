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

;; [[file:../../../gwp.note::4e63ecbf][4e63ecbf]]
;;; editor/core/config.el -*- lexical-binding: t; -*-

;;;###autoload
;; https://www.emacswiki.org/emacs/CopyingWholeLines
(defun gwp::copy-current-line (&optional arg)
  (interactive "p")
  (let ((buffer-read-only t)
        (kill-read-only-ok t))
    (kill-whole-line arg))
  )

;;;###autoload
(defun gwp::meow-insert-at-the-beginning ()
  (interactive)
  (if mark-active
      (call-interactively #'meow-insert-mode)
    (meow-join 1)
    (meow-append)))

;;;###autoload
(defun gwp::meow-insert-at-the-end ()
  (interactive)
  (if mark-active
      (call-interactively #'meow-insert-mode)
    (meow-line 1)
    (meow-append)))

;;;###autoload
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Matching-parentheses.html
(defun gwp::match-paren (arg)
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (call-interactively #'meow-block))))

;;;###autoload
(defun gwp::meow-change-to-the-end ()
  (interactive)
  (meow-insert)
  (kill-line))

;;;###autoload
(defun gwp::meow-change-whole-line ()
  (interactive)
  (call-interactively #'crux-move-beginning-of-line)
  (call-interactively #'gwp::meow-change-to-the-end))
;; 4e63ecbf ends here

;; [[file:../../../gwp.note::e08c1132][e08c1132]]
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
   ;; 常规移动操作
   '("h" . meow-left)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   ;; 常规编辑操作
   '("i" . meow-insert)
   '("I" . gwp::meow-insert-at-the-beginning)
   '("a" . meow-append)
   '("A" . gwp::meow-insert-at-the-end)
   '("x" . meow-kill)                   ; 同 vi, 剪入 king-ring, 无选区时等效于 C-x 按键
   '("y" . meow-save)                   ; 同 vi, 复制到 king-ring
   '("c" . meow-change)                 ; 同 vi, 删除选区内容, 无选区时等效于 C-c 按键
   '("C" . gwp::meow-change-whole-line) ; 同 vi, 修改整行
   '("K" . gwp::meow-change-to-the-end) ; 像 C-k, 但进入 insert mode
   '("p" . meow-yank)
   '("O" . meow-open-above)
   '("J" . crux-top-join-line)      ; 同vi, 合并下一行至当前行
   '("r" . meow-change-char)        ; 删除当前字符或选区(不进入 kill-ring), 同时进入 insert state
   '("d" . meow-delete)             ; 删除当前字符或选区(不进入 kill-ring)
   '("D" . meow-kill-whole-line)
   ;; 选区扩展操作
   '("." . meow-line)                ; 向下扩选一行, 按 "-." 向上扩选
   '("e" . meow-next-word)           ; 向前扩选, 以 word 为单位
   '("E" . meow-next-symbol)         ; 向前扩选, 以 symbol 为单位 (包括连字符等)
   '("b" . meow-back-word)           ; 反向操作, 等效于 "-e"
   '("B" . meow-back-symbol)         ; 反向操作, 等效于 "-E"
   '("o" . meow-reverse)             ; 反转选区方向. 若无选区, 则相当于 vi 中为 o
   '("u" . gwp::undo-dwim)
   '("U" . meow-pop-selection)
   ;; 搜索与跳转
   '("/" . meow-visit)            ; 快速搜索
   '("n" . meow-search)           ; 向选区方向搜索, 可按 o 键改变当前选区方向
   '("V" . meow-line-expand)
   '("f" . meow-find)             ; 含搜索字符
   '("t" . meow-till)             ; 不含搜索字符
   '("m" . point-to-register)
   '("`" . jump-to-register)
   '("z" . avy-goto-char-in-line)
   ;; 常规选择
   '("%" . gwp::match-paren)
   '("*" . meow-mark-symbol)
   ;; '("q" . meow-mark-word)
   '("s" . meow-inner-of-thing)
   '("S" . meow-bounds-of-thing)
   '(";" . meow-cancel-selection)
   '("v" . meow-cancel-selection) ; 仿 vi
   '("G" . meow-grab)             ; 相当于 vi 中的 visual mode
   '("C-v" . meow-grab)
   ;; 特殊功能
   '("]" . sp-unwrap-sexp)
   '("$" . ispell-word)
   '("Z" . repeat-complex-command)      ; 所有需要 minibuffer 输入的命令
   )

  ;; 当无选区时执行的功能
  (setq
   meow-selection-command-fallback
   '(
     (meow-reverse . meow-open-below)
     (meow-kill . meow-keypad-start)    ; for C-x
     (meow-change . meow-keypad-start)  ; for C-c
     (meow-save . gwp::copy-current-line)
     ;; (meow-pop-selection . meow-pop-grab)
     (meow-beacon-change . meow-beacon-change-char)
     (meow-cancel-selection . meow-right-expand) ; 仿vi, 取消选择或扩选
     )))

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

  ;; meow 的优先级很高, 这里定义按键为覆盖 dired 中的设置
  (map! :map meow-motion-state-keymap
        "," (general-simulate-key "s-,")
        "g" (general-simulate-key "s-g")
        "w" (general-simulate-key "s-w")
        "j" (general-simulate-key "C-n")
        "k" (general-simulate-key "C-p")
        "h" (general-simulate-key "C-b")
        "l" (general-simulate-key "C-f")
        ;; 将功能固化后不方便其它模块中覆盖
        ;; "h" #'meow-left
        ;; "l" #'meow-right
        ;; "b" #'meow-back-word
        ;; "B" #'meow-back-symbol
        ;; "f" #'meow-find
        ;; "t" #'meow-till
        ;; "s" #'meow-inner-of-thing
        ;; "S" #'meow-bounds-of-thing
        ;; "*" #'meow-mark-symbol
        ;; ";" #'meow-cancel-selection
        "y" #'meow-save
        "v" #'meow-cancel-selection
        )

  (gwp::goto-leader-def
    :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
    "g" (general-simulate-key "M-<" :which-key "goto first line")
    "e" (general-simulate-key "M->" :which-key "goto last line")
    "h" (general-simulate-key "C-a" :which-key "goto the beggining of line")
    "l" (general-simulate-key "C-e" :which-key "goto the end of line")
    "." 'goto-line
    ;; "g" '(beginning-of-buffer :which-key "goto first line")
    ;; "e" '(end-of-buffer :which-key "goto last line")
    ;; "l" '(end-of-line :which-key "goto the end of line")
    ;; "h" '(beginning-of-line :which-key "goto the beginning of line")
    ;; "d" '(+lookup/definition :which-key "Jump to definition")
    ;; "f" '(+lookup/file :which-key "Locate file")
    )

  ;; (meow-motion-overwrite-define-key
  ;;  '("j" . meow-next)
  ;;  '("k" . meow-prev)
  ;;  )

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
        "x" #'meow-keypad-start
        "c" #'meow-keypad-start
        "h" #'help-command))

;; 比如 dired, magit 生成的 buffer, 也许单独处理更好?
(defun meow/setup-motion ()
  (meow-motion-overwrite-define-key
   '("j"  "meow-next")
   '("k"  "meow-prev")
   )
  (when (featurep! :editor meow +leader)
    (meow-motion-overwrite-define-key
     '("\\ j" "H-j")
     '("\\ k" "H-k")))
  )

(use-package! meow
  :hook (doom-init-modules . meow-global-mode)
  :demand t
  :custom
  ;; 扩选指示字符显示延时
  (meow-expand-hint-remove-delay 5.0)
  ;; 默认在 org 中不显示扩选指示字符
  (meow-expand-exclude-mode-list nil)
  ;; (meow-cursor-type-normal 'hbar)
  :config
  ;; 便于区分选区
  ;; (setq meow-cursor-type-normal '(hbar . 3))
  ;; (setq meow-cursor-type-region-cursor 'bar)
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
