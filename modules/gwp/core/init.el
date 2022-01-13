;; [[file:../../../gwp.note::6e0da3f7][6e0da3f7]]
;;; editor/core/init.el -*- lexical-binding: t; -*-
(use-package general
  :config
  ;;  prevent Key sequence starts with a non-prefix key errors
  (general-auto-unbind-keys)
  ;; (general-override-mode 1)
  (general-create-definer gwp::goto-leader-def
    :keymaps 'general-override-mode-map
    :prefix gwp::goto-leader-key)
  (general-create-definer gwp::local-leader-def
    :keymaps 'general-override-mode-map
    :prefix gwp::local-leader-key)
  (general-create-definer gwp::dwim-leader-def
    :keymaps 'meow-normal-state-keymap
    :prefix gwp::dwim-leader-key)
  )

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
