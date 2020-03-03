;;; lang/org/config.el -*- lexical-binding: t; -*-

(use-package! evil-org
  :when (featurep! :editor evil +everywhere)
  :hook (org-mode . evil-org-mode)
  :init
  (defvar evil-org-retain-visual-state-on-shift t)
  ;; 添加item项, 保持和spacemacs习惯一致
  (defvar evil-org-special-o/O '(table-row item))
  (defvar evil-org-use-additional-insert t)
  :config
  (evil-org-set-key-theme)
  (add-hook! 'org-tab-first-hook :append
             ;; Only fold the current tree, rather than recursively
             #'+org-cycle-only-current-subtree-h
             ;; Clear babel results if point is inside a src block
             #'+org-clear-babel-results-h)
  (map! :map evil-org-mode-map
        :ni [C-return]   #'+org/insert-item-below
        :ni [C-S-return] #'+org/insert-item-above
        ;; navigate table cells (from insert-mode)
        :i "C-l" (general-predicate-dispatch 'org-end-of-line
                   (org-at-table-p) 'org-table-next-field)
        :i "C-h" (general-predicate-dispatch 'org-beginning-of-line
                   (org-at-table-p) 'org-table-previous-field)
        :i "C-k" (general-predicate-dispatch 'org-up-element
                   (org-at-table-p) '+org/table-previous-row)
        :i "C-j" (general-predicate-dispatch 'org-down-element
                   (org-at-table-p) 'org-table-next-row)
        ;; moving/(de|pro)moting subtress & expanding tables (prepend/append columns/rows)
        :ni "C-S-l" #'org-shiftright
        :ni "C-S-h" #'org-shiftleft
        :ni "C-S-k" #'org-shiftup
        :ni "C-S-j" #'org-shiftdown
        ;; more intuitive RET keybinds
        :i [return] #'org-return-indent
        :i "RET"    #'org-return-indent
        :n [return] #'+org/dwim-at-point
        :n "RET"    #'+org/dwim-at-point
        ;; more vim-esque org motion keys (not covered by evil-org-mode)
        :m "]h"  #'org-forward-heading-same-level
        :m "[h"  #'org-backward-heading-same-level
        :m "]l"  #'org-next-link
        :m "[l"  #'org-previous-link
        :m "]c"  #'org-babel-next-src-block
        :m "[c"  #'org-babel-previous-src-block
        :n "gQ"  #'org-fill-paragraph
        :n "gr"  #'org-ctrl-c-ctrl-c
        :n "gR"  #'org-babel-execute-buffer
        ;; sensible vim-esque folding keybinds
        :n "za"  #'+org/toggle-fold
        :n "zA"  #'org-shifttab
        :n "zc"  #'+org/close-fold
        :n "zC"  #'outline-hide-subtree
        :n "zm"  #'+org/hide-next-fold-level
        :n "zn"  #'org-tree-to-indirect-buffer
        :n "zo"  #'+org/open-fold
        :n "zO"  #'outline-show-subtree
        :n "zr"  #'+org/show-next-fold-level
        :n "zR"  #'outline-show-all
        :n "zi"  #'org-toggle-inline-images

        :map org-read-date-minibuffer-local-map
        "C-h"   (λ! (org-eval-in-calendar '(calendar-backward-day 1)))
        "C-l"   (λ! (org-eval-in-calendar '(calendar-forward-day 1)))
        "C-k"   (λ! (org-eval-in-calendar '(calendar-backward-week 1)))
        "C-j"   (λ! (org-eval-in-calendar '(calendar-forward-week 1)))
        "C-S-h" (λ! (org-eval-in-calendar '(calendar-backward-month 1)))
        "C-S-l" (λ! (org-eval-in-calendar '(calendar-forward-month 1)))
        "C-S-k" (λ! (org-eval-in-calendar '(calendar-backward-year 1)))
        "C-S-j" (λ! (org-eval-in-calendar '(calendar-forward-year 1))))
  )
