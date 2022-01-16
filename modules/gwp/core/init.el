;; [[file:../../../gwp.note::6e0da3f7][6e0da3f7]]
;;; editor/core/init.el -*- lexical-binding: t; -*-
(use-package! general
  :after meow
  :config
  ;;  prevent Key sequence starts with a non-prefix key errors
  (general-auto-unbind-keys)
  ;; 定义 "," 及 "g", "w" 开头的按键序列.
  (general-create-definer gwp::goto-leader-def
    :keymaps 'general-override-mode-map
    :prefix gwp::goto-leader-key)
  (general-create-definer gwp::local-leader-def
    :keymaps 'general-override-mode-map
    :prefix gwp::local-leader-key)
  (general-create-definer gwp::dwim-leader-def
    :keymaps 'meow-normal-state-keymap
    :prefix gwp::dwim-leader-key)

  ;; 方便定义在 Insert 状态下的一些编辑命令
  (general-create-definer gwp::text-edit-def
    ;; :prefix "C-c"
    :keymaps '(meow-insert-state-keymap))

  ;; 用于 help 及只读类文件
  (general-create-definer gwp::text-view-def
    :keymaps '(meow-motion-state-keymap meow-normal-state-keymap))

  ;; 高优先级
  ;; (general-create-definer gwp::local-def :keymaps 'local)

  ;; 取消某些容易误按, 不习惯的键
  (general-unbind "C-v" "C-z"))

;; Rewriting the doom-localleader-key! macro to add support for meow mode
(when (featurep! :gwp core +leader)
  (defmacro define-localleader-key! (&rest args)
    `(progn
       (general-define-key
        :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
        :major-modes t
        :prefix doom-localleader-key
        ,@args)
       (general-define-key
        :keymaps 'meow-insert-state-keymap
        :major-modes t
        :prefix doom-localleader-alt-key
        ,@args)))
  )
;; 6e0da3f7 ends here
