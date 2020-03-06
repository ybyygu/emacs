;; [[file:~/Workspace/Programming/emacs/doom.note::*基本设置][基本设置:1]]
(setq org-blank-before-new-entry nil)
(setq org-default-notes-file (concat org-directory "/life.note"))

;; 保留以前的 Alt-Return 键行为, Alt-Return
(org-defkey org-mode-map [(meta return)] 'org-meta-return)

;; doom 默认 src 中不保留缩进.
(setq org-src-preserve-indentation nil)

;; 禁用字词检查, 需要了再开
(remove-hook! 'org-mode-hook #'flyspell-mode)
(flyspell-mode 0)
;; 基本设置:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*按键行为][按键行为:1]]
(defun gwp/new-memo (arg)
  "Insert a new org-mode memo entry under heading at point."

  (interactive "P")

  (call-interactively 'evil-open-below)
  (insert "** ")
  (call-interactively 'org-time-stamp-inactive)
  (insert " ")
  )

;; 经常按错这个键, 禁用之 (Ctrl-c ;)
(put 'org-toggle-comment 'disabled t)

(map! :map org-mode-map
      :n "gh" #'org-up-element
      :n "gl" #'org-down-element ; doom中默认为: evil-lion-left
      :n "gk" #'org-previous-visible-heading
      :n "gj" #'org-next-visible-heading
      :n "M-l" #'org-metaright   ; doom中默认为 demote-subtree
      :n "M-h" #'org-metaleft    ; doom中默认为 promote-subtree
      )

(map! :map org-mode-map
      :localleader
      ;; FIXME: 与doom/org定义有冲突
      (:prefix ("s" . "Subtree")
               :desc "Demote" "l" #'org-demote-subtree
               :desc "Promote" "h" #'org-promote-subtree
               :desc "Archive" "A" #'org-archive-subtree
               :desc "Narrow" "n" #'org-toggle-narrow-to-subtree
               )
      (:prefix ("SPC" . "Special")
               :desc "org-ctrl-c-star" "s" #'org-ctrl-c-star ; 方便盲按
               :desc "Insert new memo entry" "m" #'gwp/new-memo ; 简化操作
               )
      )
(map! :map org-mode-map
      :localleader
      (:prefix ("g" . "Goto")
               :desc "Goto the previous position"  "p" #'org-mark-ring-goto
               :desc "Jump to org heading"  "j" #'counsel-org-goto
               :desc "Goto named src block" "b" #'org-babel-goto-named-src-block
               )
      )
;; 按键行为:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*tangle][tangle:1]]
(map! :map org-mode-map
      :localleader
      (:prefix ("b" . "org-babel")
        :desc "insert header argument" "i" #'org-babel-insert-header-arg
        :desc "tangle blocks at point" "b" #'gwp/org-babel-tangle-blocks
        :desc "tangle blocks in subtree" "t" #'gwp/org-tangle-subtree
        :desc "tangle blocks in buffer" "T" #'org-babel-tangle
        )
      ;; 为了顺应spacemacs中的设置, 保留spc-ob 按键
      :leader
      :desc "tangle blocks at point" "o b" #'gwp/org-babel-tangle-blocks
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

;; narrow to subtree before calling org-babel-tangle
(defun gwp/org-tangle-subtree ()
  "tange src blocks in current subtree"
  (interactive)
  (org-narrow-to-subtree)
  (org-babel-tangle)
  (widen)
  )
;; tangle:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*dwim-at-point][dwim-at-point:1]]
(defun gwp/dwim-at-point ()
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- clock: update its time.
- headline: toggle latex fragments and inline images underneath.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: 改为编辑代码, edit-special
- statistics-cookie: update it.
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive)
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    ;; skip over unimportant contexts
    (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
      (setq context (org-element-property :parent context)
            type (org-element-type context)))
    (pcase type
      (`headline
       (cond ((and (fboundp 'toc-org-insert-toc)
                   (member "TOC" (org-get-tags)))
              (toc-org-insert-toc)
              (message "Updating table of contents"))
             ((string= "ARCHIVE" (car-safe (org-get-tags)))
              (org-force-cycle-archived))
             ((or (org-element-property :todo-type context)
                  (org-element-property :scheduled context))
              (org-todo
               (if (eq (org-element-property :todo-type context) 'done)
                   (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                       'todo)
                 'done)))
             (t
              (+org--refresh-inline-images-in-subtree)
              (org-clear-latex-preview)
              (org-latex-preview '(4)))))

      (`clock (org-clock-update-time-maybe))

      (`footnote-reference
       (org-footnote-goto-definition (org-element-property :label context)))

      (`footnote-definition
       (org-footnote-goto-previous-reference (org-element-property :label context)))

      ((or `planning `timestamp)
       (org-follow-timestamp-link))

      ((or `table `table-row)
       (if (org-at-TBLFM-p)
           (org-table-calc-current-TBLFM)
         (ignore-errors
           (save-excursion
             (goto-char (org-element-property :contents-begin context))
             (org-call-with-arg 'org-table-recalculate (or arg t))))))

      (`table-cell
       (org-table-blank-field)
       (org-table-recalculate)
       (when (and (string-empty-p (string-trim (org-table-get-field)))
                  (bound-and-true-p evil-local-mode))
         (evil-change-state 'insert)))

      (`babel-call
       (org-babel-lob-execute-maybe))

      (`statistics-cookie
       (save-excursion (org-update-statistics-cookies nil)))

      ((or `src-block `inline-src-block)
       ;; 还是挺方便的
       (org-edit-special))

      ((or `latex-fragment `latex-environment)
       (org-latex-preview))

      (`link
       (let* ((lineage (org-element-lineage context '(link) t))
              (path (org-element-property :path lineage)))
         (if (or (equal (org-element-property :type lineage) "img")
                 (and path (image-type-from-file-name path)))
             (+org--refresh-inline-images-in-subtree)
           (org-open-at-point))))

      ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
       (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
         (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

      (_ (+org--refresh-inline-images-in-subtree)))))
;; dwim-at-point:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*screenshot][screenshot:1]]
;; 原版的容易把emacs挂住, 由于shell-coomand-to-string
(defun gwp/org-download-screenshot ()
  "Capture screenshot and insert the resulting file.
The screenshot tool is determined by `org-download-screenshot-method'."
  (interactive)
  (require 'org-download)
  (let ((default-directory "~"))
    (make-directory (file-name-directory org-download-screenshot-file) t)
    ;; (shell-command-to-string
    ;;  (format org-download-screenshot-method
    ;;          org-download-screenshot-file))
    (start-process-shell-command
     "org-download-edit"
     "org-download-edit"
     (format "txclip paste --image -o %s" org-download-screenshot-file)
     ))
  (when (file-exists-p org-download-screenshot-file)
    (org-download-image org-download-screenshot-file)
    (delete-file org-download-screenshot-file)))


(defun gwp/org-image-attributes-default (&optional caption)
  "default image attributes: caption, name label, width ..."
  (format (concat
           ;; #+DOWNLOAD mark: for easy to delete using org-download
           (format "#+DOWNLOADED: @ %s\n" (format-time-string "%Y-%m-%d %H:%M:%S"))
           (concat  "#+caption: " (read-string "Caption: " caption) "\n")
           ;; set unique figure name
           (format "#+name: fig:%s\n" (substring (org-id-new) 0 8))
           ;; unit in px; for displaying in org-mode
           "#+attr_org: :width 800\n"
           ;; unit in cm; for exporting as odt
           "#+attr_odt: :width 10"
           )
          )
  )

(defun gwp/org-insert-image-attributes (&optional caption)
  "insert image attributes such as caption and labels"
  (interactive)
  (insert (gwp/org-image-attributes-default caption))
  )

(defun gwp/org-download-annotate (link)
  "Annotate LINK with the time of download."
  (gwp/org-image-attributes-default)
  )

(use-package! org-download
              :commands
              org-download-delete
              org-download-yank
              org-download-screenshot
              :config
              (progn
                (setq org-download-method 'attach
                      ;; org-download-annotate-function 'gwp/org-download-annotate ; 有点问题, 暂时禁用
                      ;; org-download-image-html-width 900 ; in px
                      ;; org-download-image-latex-width 16 ; in cm
                      org-download-screenshot-method
                      (cond ((executable-find "txclip")  "txclip paste --image -o %s")
                            ((executable-find "deepin-screenshot")  "deepin-screenshot -s %s")
                            ((executable-find "scrot") "scrot -s %s"))
                      )))
;; screenshot:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*init][init:1]]
;; helper functions for literate programming
;; taking from: https://github.com/grettke/help/blob/master/Org-Mode_Fundamentals.org
(defun help/set-org-babel-default-header-args (property value)
  "Easily set system header arguments in org mode.

PROPERTY is the system-wide value that you would like to modify.

VALUE is the new value you wish to store.

Attribution: URL `http://orgmode.org/manual/System_002dwide-header-arguments.html#System_002dwide-header-arguments'"
  (setq org-babel-default-header-args
        (cons (cons property value)
              (assq-delete-all property org-babel-default-header-args))))

(help/set-org-babel-default-header-args :padline "yes")
(help/set-org-babel-default-header-args :mkdirp "yes")
(help/set-org-babel-default-header-args :comments "link")
;; init:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*enter][enter:1]]
;; 禁用代码着色, 影响速度
(setq org-src-fontify-natively nil)

;; 编辑代码时在下方新开窗口
;;(setq org-src-window-setup 'split-window-below)
(setq org-src-window-setup 'current-window)
;;(setq org-src-window-setup 'reorganize-frame)
;;(setq org-src-window-setup 'other-frame)

;; 进入代码编辑模式, 改成容易按的
(map! :map org-mode-map
      :ni "C-c ;" #'org-edit-special
      :ni "C-c C-;" #'org-edit-special
      :localleader ";" #'org-edit-special
      )
;; enter:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*edit][edit:1]]
;; 用于激活 localleader
(add-hook 'org-src-mode-hook #'evil-normalize-keymaps)

;; 默认的不太好按. 不能用C-c C-c, 容易与别的模块冲突.
(map! :map org-src-mode-map
      "C-c ;"   #'org-edit-src-exit  ; 保存退出
      "C-c C-;" #'org-edit-src-exit  ; 保存退出
      "C-c C-k" #'org-edit-src-abort ; 放弃修改
      )
(map! :map org-src-mode-map
      :localleader
      ";" #'org-edit-src-exit
      "c" #'org-edit-src-exit
      "k" #'org-edit-src-abort
      )
(map! :map rust-mode-map
      :localleader
      "=" #'rust-format-buffer
      )
;; edit:1 ends here
