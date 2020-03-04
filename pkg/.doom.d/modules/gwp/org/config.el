;;; lang/org/config.el -*- lexical-binding: t; -*-

(use-package! evil-org
  :when (featurep! :editor evil +everywhere)
  :hook (org-mode . evil-org-mode)
  :init
  (defvar evil-org-retain-visual-state-on-shift t)
  ;; spacemacs中有item项, 按o/O会接续item, 而不是普通的换行. doom中可以用ctrl-enter
  (defvar evil-org-special-o/O '(table-row))
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
        ;; :i "C-l" (general-predicate-dispatch 'org-end-of-line
        ;;            (org-at-table-p) 'org-table-next-field)
        ;; :i "C-h" (general-predicate-dispatch 'org-beginning-of-line
        ;;            (org-at-table-p) 'org-table-previous-field)
        ;; :i "C-k" (general-predicate-dispatch 'org-up-element
        ;;            (org-at-table-p) '+org/table-previous-row)
        ;; :i "C-j" (general-predicate-dispatch 'org-down-element
        ;;            (org-at-table-p) 'org-table-next-row)
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

(use-package! org-pdfview
  :when (featurep! :tools pdf)
  :commands org-pdfview-open
  :init
  (after! org
    (delete '("\\.pdf\\'" . default) org-file-apps)
    ;; org links to pdf files are opened in pdf-view-mode
    (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (_file link) (org-pdfview-open link))))
    ;; support for links to specific pages
    (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . (lambda (_file link) (org-pdfview-open link))))))

(defun +org-init-babel-h ()
  (setq org-src-preserve-indentation t  ; use native major-mode indentation
        org-src-tab-acts-natively t     ; we do this ourselves
        org-confirm-babel-evaluate t    ; 太危险
        ;; Show src buffer in popup, and don't monopolize the frame
        org-src-window-setup 'other-window ; 这个设置重复了
        )

  ;; 可以把之前的设置放这里
  ;; I prefer C-c C-c over C-c ' (more consistent)
  ;; (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

  (defadvice! +org-fix-newline-and-indent-in-src-blocks-a ()
    "Mimic `newline-and-indent' in src blocks w/ lang-appropriate indentation."
    :after #'org-return-indent
    (when (org-in-src-block-p t)
      (org-babel-do-in-edit-buffer
       (call-interactively #'indent-for-tab-command)))
    )

  ;; Refresh inline images after executing src blocks (useful for plantuml or
  ;; ipython, where the result could be an image)
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
  )

(defun +org-init-babel-lazy-loader-h ()
  "Load babel libraries lazily when babel blocks are executed."
  (defun +org--babel-lazy-load (lang)
    (cl-check-type lang symbol)
    (or (run-hook-with-args-until-success '+org-babel-load-functions lang)
        (require (intern (format "ob-%s" lang)) nil t)
        (require lang nil t)
        )
    )

  (defadvice! +org--src-lazy-load-library-a (lang)
    "Lazy load a babel package to ensure syntax highlighting."
    :before #'org-src--get-lang-mode
    (or (cdr (assoc lang org-src-lang-modes))
        (+org--babel-lazy-load lang)))

  ;; This also works for tangling and exporting
  (defadvice! +org--babel-lazy-load-library-a (info)
    "Load babel libraries lazily when babel blocks are executed."
    :after-while #'org-babel-confirm-evaluate
    (let* ((lang (nth 0 info))
           (lang (cond ((symbolp lang) lang)
                       ((stringp lang) (intern lang))))
           (lang (or (cdr (assq lang +org-babel-mode-alist))
                     lang)))
      (when (and lang
                 (not (cdr (assq lang org-babel-load-languages)))
                 (+org--babel-lazy-load lang))
        (when (assq :async (nth 2 info))
          ;; ob-async has its own agenda for lazy loading packages (in the
          ;; child process), so we only need to make sure it's loaded.
          (require 'ob-async nil t))
        (add-to-list 'org-babel-load-languages (cons lang t)))
      t))

  (defadvice! +org--noop-org-babel-do-load-languages-a (&rest _)
    :override #'org-babel-do-load-languages
    (message
     (concat "`org-babel-do-load-languages' is redundant with Doom's lazy loading mechanism for babel "
             "packages. There is no need to use it, so it has been disabled")))
  )

(defun +org-init-appearance-h ()
  "Configures the UI for `org-mode'."
  (setq org-indirect-buffer-display 'current-window ; 默认不是这个
        org-enforce-todo-dependencies t
        org-entities-user
        '(("flat"  "\\flat" nil "" "" "266D" "♭")
          ("sharp" "\\sharp" nil "" "" "266F" "♯"))
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-footnote-auto-label 'plain
        org-hide-leading-stars t
        org-hide-leading-stars-before-indent-mode t
        org-image-actual-width nil
        org-list-description-max-indent 4
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success))
        org-startup-indented t
        org-tags-column 0
        org-use-sub-superscripts '{})

  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3))
        ;; Without this, completers like ivy/helm are only given the first level of
        ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
        ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
        ;; target! e.g. FILE/Tasks/heading/subheading
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  ;; Fontify latex blocks and entities, but not natively -- that's too slow
  (setq org-highlight-latex-and-related '(latex script entities))
  (plist-put! org-format-latex-options
              :scale 1.5         ; larger previews
              :foreground 'auto  ; match the theme foreground
              :background 'auto) ; ... and its background

  ;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
  ;;      underlying faces like the `org-todo' face does, so we define our own
  ;;      intermediary faces that extend from org-todo.
  (with-no-warnings
    (custom-declare-face '+org-todo-active '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold '((t (:inherit (bold warning org-todo)))) ""))
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; An ongoing project that cannot be completed in one step
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something is holding up this task; or it is paused
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")) ; Task was completed
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)))

  ;; (defadvice! +org-display-link-in-eldoc-a (orig-fn &rest args)
  ;;   "Display full link in minibuffer when cursor/mouse is over it."
  ;;   :around #'org-eldoc-documentation-function
  ;;   (or (when-let (link (org-element-property :raw-link (org-element-context)))
  ;;         (format "Link: %s" link))
  ;;       (apply orig-fn args)))

  ;; Automatic indent detection in org files is meaningless
  (cl-pushnew 'org-mode doom-detect-indentation-excluded-modes :test #'eq)

  (set-pretty-symbols! 'org-mode
    :name "#+NAME:"
    :src_block "#+BEGIN_SRC"
    :src_block_end "#+END_SRC")
  )

(use-package! org
  :preface
  ;; Change org defaults (should be set before org loads)
  (setq org-directory "~/Notes/"
        ;; 默认的data/, 保留原样的好
        ;; org-attach-id-dir ".attach/"
        org-publish-timestamp-directory (concat doom-cache-dir "org-timestamps/")
        org-preview-latex-image-directory (concat doom-cache-dir "org-latex/")
        )

  ;; eldoc没多大用
  ;; (add-hook 'org-mode-local-vars-hook #'eldoc-mode)

  (add-hook! 'org-mode-hook
             ;; #'doom-disable-show-paren-mode-h
             ;; ;; disable `show-trailing-whitespace'; shows a lot of false positives
             ;; #'doom-disable-show-trailing-whitespace-h
             ;; #'+org-enable-auto-reformat-tables-h
             ;; #'+org-enable-auto-update-cookies-h
             ;; #'+org-unfold-to-2nd-level-or-point-h
             )

  (add-hook! 'org-load-hook
             #'+org-init-appearance-h
             ;; #'+org-init-agenda-h
             ;; #'+org-init-attachments-h
             #'+org-init-babel-h
             #'+org-init-babel-lazy-loader-h
             ;; #'+org-init-capture-defaults-h
             ;; #'+org-init-capture-frame-h
             ;; #'+org-init-custom-links-h
             ;; #'+org-init-export-h
             ;; #'+org-init-hacks-h
             ;; #'+org-init-keybinds-h
             ;; #'+org-init-popup-rules-h
             ;; #'+org-init-protocol-h
             ;; #'+org-init-protocol-lazy-loader-h
             ;; #'+org-init-smartparens-h
             )

  ;; In case the user has eagerly loaded org from their configs
  (when (and (featurep 'org)
             (not doom-reloading-p)
             (not byte-compile-current-file))
    (message "`org' was already loaded by the time lang/org loaded, this may cause issues")
    (run-hooks 'org-load-hook)
    )

  :config
  ;; (add-hook 'org-open-at-point-functions #'doom-set-jump-h)
  )
