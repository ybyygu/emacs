;; [[file:../../../gwp.note::b28b06dc][b28b06dc]]
(require 'org)
(setq org-blank-before-new-entry nil)
(setq org-default-notes-file (concat org-directory "/life.note"))

;; 禁用字词检查, 需要了再开
(remove-hook! 'org-mode-hook #'flyspell-mode)
(remove-hook! 'text-mode-hook #'spell-fu-mode)

(setq auto-mode-alist
      (cons '("\\.note$" . org-mode) auto-mode-alist))
;; b28b06dc ends here

;; [[file:../../../gwp.note::7341aa84][7341aa84]]
;; 2022-01-31: 也许和按Tab 乱跳有关?
;; 2022-02-11: 应无关
;; (setq org-cycle-emulate-tab nil)
;; https://orgmode.org/manual/Clean-view.html
(setq org-startup-indented t)      ;Enable `org-indent-mode' on Org startup
(with-eval-after-load 'org-indent
  (setq org-indent-indentation-per-level 1)) ;; default = 2

;; 对齐headline中的TAGs
(setq org-tags-column -80)

;; 方便用 property 来控制 image 显示大小
(setq org-image-actual-width nil)

;; 避免误编辑
(setq org-catch-invisible-edits 'show-and-error)

;; 避免切换时闪烁
;; (setq org-startup-indented nil)
;; (remove-hook! 'org-mode-hook #'org-indent-mode)

;; 避免显示subtree之间多余的空行
(setq org-cycle-separator-lines 0)

(use-package org-superstar
  :init
  ;; ◉ ○ ◆ » ◇ ▶ ▷
  (setq org-superstar-headline-bullets-list '("☰" "▶" "▷" "»"))
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; 显示光标所在处的内容
(defun gwp::org-show-context-at-point ()
  (interactive)
  (call-interactively #'org-show-subtree)
  ;; 从下面的命令看来的
  ;; (call-interactively 'org-mark-ring-goto)
  ;; (org-show-context 'mark-goto)
  ;; (when (or (org-invisible-p) (org-invisible-p2)) (org-show-context 'mark-goto))
  (call-interactively #'org-reveal)
  )

;; 默认的为 org-reveal, 但不太好用
(map! :map org-mode-map
      "C-c C-r" #'gwp::org-show-context-at-point
      )


;; 禁用*bold*等标注的字体效果. 写代码时容易弄花显示. 比如__init__.
(setq org-fontify-emphasized-text nil)

;; (map! :map org-mode-map
;;       :n "zo" #'gwp::org-show-context-at-point
;;       :n "zc" #'org-hide-entry)
;; 7341aa84 ends here

;; [[file:../../../gwp.note::3b3471ee][3b3471ee]]
(defun gwp/org-image-attributes-default (&optional caption)
  "default image attributes: caption, name label, width ..."
  "Annotate LINK with the time of download."
  (format (concat
           (concat  "#+caption: " (read-string "Caption: " caption) "\n")
           ;; set unique figure name
           (format "#+name: fig:%s\n" (substring (org-id-new) 0 8))
           ;; unit in px; for displaying in org-mode
           "#+attr_org: :width 800\n"
           ;; unit in cm; for exporting as odt
           "#+attr_odt: :width 10\n"
           )))

(defun gwp/org-insert-image-attributes (&optional caption)
  "insert image attributes such as caption and labels"
  (interactive)
  (insert (gwp/org-image-attributes-default caption)))

(defun gwp/org-download-annotate (link)
  "Annotate LINK with the time of download."
  (gwp/org-image-attributes-default))

(use-package! org-download
  :commands
  org-download-delete
  org-download-yank
  org-download-clipboard
  :hook ((org-mode . org-download-enable)) ; 启用拖放功能
  :bind (:map org-mode-map
         ("C-c v" . org-download-clipboard))
  :config
  (setq org-download-method 'attach
        org-download-annotate-function 'gwp/org-download-annotate
        ;; org-download-image-html-width 900 ; in px
        ;; org-download-image-latex-width 16 ; in cm
        ;; 2021-09-03: 直接调用org-download-clipboard即可, 以下代码不必要
        ;; org-download-screenshot-method
        ;; (cond ((executable-find "txclip")  "txclip paste --image -o %s")
        ;;       ((executable-find "scrot") "scrot -s %s"))
        ))
;; 3b3471ee ends here

;; [[file:../../../gwp.note::3d7188a4][3d7188a4]]
(defun gwp::new-memo-time-stamp (arg)
  "Insert a new org-mode memo entry under heading at point."
  (interactive "P")
  (unless (org-at-heading-p)
    (org-up-element))
  (call-interactively 'crux-smart-open-line)
  (call-interactively 'org-insert-todo-subheading)
  (call-interactively 'org-time-stamp-inactive)
  (when (meow-normal-mode-p) (call-interactively 'meow-insert))
  (insert " "))

(defun gwp::new-item-time-stamp (arg)
  (interactive "P")
  (if (org-in-item-p)
      (progn
        (call-interactively 'org-beginning-of-item)
        (call-interactively 'org-insert-item))
    (insert "- "))
  (call-interactively 'org-time-stamp-inactive)
  (insert " ")
  (when (meow-normal-mode-p) (call-interactively 'meow-insert))
  )
;; 3d7188a4 ends here

;; [[file:../../../gwp.note::ac811d6f][ac811d6f]]
(after! smartparens
  (sp-with-modes '(org-mode)
    (sp-local-pair "/" nil :actions :rem)
    (sp-local-pair "=" nil :actions :rem)
    (sp-local-pair "~" nil :actions :rem)
    (sp-local-pair "*" nil :actions :rem)
    (sp-local-pair "_" nil :actions :rem)))
;; ac811d6f ends here

;; [[file:../../../gwp.note::bafd9bd3][bafd9bd3]]
(general-define-key :prefix-map 'gwp::org-insert-map)

(map! :map gwp::org-insert-map
      :desc "new memo entry"
      "m" #'gwp::new-memo-time-stamp ; 简化操作
      :desc "new item"
      "i" #'gwp::new-item-time-stamp
      :desc "inactive time-stamp"
      "t" #'org-time-stamp-inactive
      :desc "active time-stamp"
      "." #'org-time-stamp
      :desc "stored link"
      "l" #'org-insert-last-stored-link
      :desc "schedule"
      "s" #'org-schedule
      :desc "deadline"
      "d" #'org-deadline
      :desc "note"
      "n" #'org-add-note
      :desc "sort list"
      "S" #'org-sort-list ; 可用于给列表排序, 默认为C-c ^
      )

(gwp::local-leader-def
 :keymaps 'org-mode-map
 "i" '(:keymap gwp::org-insert-map :which-key "insert/item")
 )
;; bafd9bd3 ends here

;; [[file:../../../gwp.note::568eea25][568eea25]]
(defun gwp/org-get-refile-targets ()
  "Return the list of files currently opened in emacs"
  (delq nil
        (mapcar (lambda (x)
                  (if (and (buffer-file-name x)
                           (string-match "\\.note$"
                                         (buffer-file-name x)))
                      (buffer-file-name x)))
                (buffer-list))))

;;(setq org-refile-targets '((gwp/org-get-refile-targets :tag . "Incoming")))
(setq org-refile-targets '((gwp/org-get-refile-targets :regexp . "^* Memo")))
(setq org-refile-use-outline-path nil)

(setq org-reverse-note-order t)
(defun gwp/get-org-file-link-path ()
  (save-excursion
    (beginning-of-line)
    (search-forward "[[file:" (line-end-position))
    (if (org-in-regexp org-bracket-link-regexp 1)
        (org-link-unescape (match-string-no-properties 1)))))
;; 568eea25 ends here

;; [[file:../../../gwp.note::5ca48f79][5ca48f79]]
(defun ap/org-tree-to-indirect-buffer (&optional arg)
  "Create indirect buffer and narrow it to current subtree.
The buffer is named after the subtree heading, with the filename
appended.  If a buffer by that name already exists, it is
selected instead of creating a new buffer."
  (interactive "P")
  (let* ((new-buffer-p)
         (pos (point))
         (buffer-name (let* ((heading (org-get-heading t t))
                             (level (org-outline-level))
                             (face (intern (concat "outline-" (number-to-string level))))
                             (heading-string (propertize (org-link-display-format heading)
                                                         'face face)))
                        (concat heading-string "::" (buffer-name))))
         (new-buffer (or (get-buffer buffer-name)
                         (prog1 (condition-case nil
                                    (make-indirect-buffer (current-buffer) buffer-name 'clone)
                                  (error (make-indirect-buffer (current-buffer) buffer-name)))
                           (setq new-buffer-p t)))))
    (switch-to-buffer new-buffer)
    (when new-buffer-p
      ;; I don't understand why setting the point again is necessary, but it is.
      (goto-char pos)
      (rename-buffer buffer-name)
      (org-narrow-to-subtree))))

(advice-add 'org-tree-to-indirect-buffer :override 'ap/org-tree-to-indirect-buffer)
;; 5ca48f79 ends here

;; [[file:../../../gwp.note::ab0515d6][ab0515d6]]
;;;###autoload
(defun gwp::org-babel-narrow-to-tangle-heading ()
  "narrow至当前代码块对应的 tangle 文件所在级别"
  (interactive)
  (let ((tangle-file (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info 'light)))))
        (start-position (point))
        offset)
    ;; :tangle no 不能算
    (if (and tangle-file (not (string= tangle-file "no")))
        (save-excursion
          (if (search-backward (format ":tangle %s" tangle-file) nil t)
              (progn
                (setq offset (- start-position (point)))
                (ap/org-tree-to-indirect-buffer)
                (forward-char offset)
                (message "narrowed to heading: %s" tangle-file))
            (message "no root headline found")))
      (message "narrowed to headline at point")
      (ap/org-tree-to-indirect-buffer))))
;; ab0515d6 ends here

;; [[file:../../../gwp.note::a393f96d][a393f96d]]
(general-define-key :prefix-map 'gwp::org-subtree-map)

(map! :map gwp::org-subtree-map
      :desc "Demote"
      "l" #'org-demote-subtree
      :desc "Promote"
      "h" #'org-promote-subtree
      :desc "Archive"
      "A" #'org-archive-subtree
      :desc "Narrow"
      "n" #'gwp::org-babel-narrow-to-tangle-heading
      ;; 仿SPC-s-s
      :desc "Goto"
      "s" #'counsel-org-goto
      :desc "Goto (all)"
      "S" #'counsel-org-goto-all
      :desc "Toggle org-sidebar-tree"
      "t" #'org-sidebar-tree-toggle
      :desc "refile subtree"
      "r" #'org-refile
      :desc "refile but preserve subtree"
      "R" #'org-refile-copy
      )

(gwp::local-leader-def
 :keymaps 'org-mode-map
 "s" '(:keymap gwp::org-subtree-map :which-key "subtree/search")
 )
;; a393f96d ends here

;; [[file:../../../gwp.note::*init][init:1]]
;; 不缩进org-src块中的代码.
;; 注意: 不直接设置为"org-src-preserve-indentation t",
;; 只设置org-edit-src-content-indentation为0, 这样仅影响编辑的org, 不影响tangle
;; 出的代码. 以前的org文档可以逐步调回来
(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0) ;Default = 2

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

;; 几个重要的header args:
(help/set-org-babel-default-header-args :padline "yes")
(help/set-org-babel-default-header-args :mkdirp "yes")
(help/set-org-babel-default-header-args :comments "link")
;; init:1 ends here

;; [[file:../../../gwp.note::0d8e352a][0d8e352a]]
;; activate languages for evaluation
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t) ;; this is the default
   (python . t)
   (shell . t)
   (sqlite . t)
   (R . t)
   (restclient . t)                     ; 测试 http API
   (gnuplot . t)                        ; 需要安装 gnuplot 对应的两个包
   ))
;; 0d8e352a ends here

;; [[file:../../../gwp.note::624e5b7f][624e5b7f]]
;; 禁用代码着色, 影响速度
;; (setq org-src-fontify-natively nil)

;; 编辑代码时在下方新开窗口
;;(setq org-src-window-setup 'split-window-below)
(setq org-src-window-setup 'current-window)
;(setq org-src-window-setup 'reorganize-frame)
;;(setq org-src-window-setup 'other-frame)
;; 624e5b7f ends here

;; [[file:../../../gwp.note::*toml][toml:1]]
;; Add convenience lang alias for markdown blocks
(add-to-list 'org-src-lang-modes '("toml" . conf-toml))
;; toml:1 ends here

;; [[file:../../../gwp.note::8aa4aca8][8aa4aca8]]
(defhydra gwp/org-jump-block ()
  "jump to org blocks"
  ("n" org-next-block "next block")
  ("p" org-previous-block "prev block")
  ("q" nil "quit")
  )

(defhydra gwp/org-jump-link ()
  "jump to org links"
  ("n" org-next-link "next link")
  ("p" org-previous-link "prev link")
  ("q" nil "quit")
  )
;; 8aa4aca8 ends here

;; [[file:../../../gwp.note::fa928b1c][fa928b1c]]
(defun gwp/org-babel-tangle-jump-to-file ()
  "Jump to tangle file for the source block at point."
  (interactive)
  (let ((mid (point))
        (element (org-element-at-point))
        (body-start (save-excursion
                      (progn
                        (org-babel-goto-src-block-head)
                        (next-line)
                        (point)
                        )))
        (tangle-file (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info 'light)))))
        offset)
    (if tangle-file
        (let ((block-name (org-element-property :name element))
              (tangle-file (expand-file-name tangle-file)))
          (if (file-readable-p tangle-file)
              (progn
                ;; open tangled file
                (find-file tangle-file)
                ;; if code block has a name, we jump to that block
                (when block-name
                  (beginning-of-buffer)   ; if point restored, the searching could fail
                  (when (search-forward (format "::%s" block-name) nil t)
                    (next-line)
                    (beginning-of-line)
                    (setq offset (- mid body-start))
                    (forward-char offset)
                    (recenter)
                    )))
            (error "Cannot open tangle file %S" tangle-file)))
      (message "not in source block"))))
;; fa928b1c ends here

;; [[file:../../../gwp.note::9b40c7cf][9b40c7cf]]
(defun gwp/org-babel-tangle-jump-to-org ()
  "Jump from a tangled code file to the related Org mode file."

  (require 'ol)
  (interactive)
  (let ((mid (point))
	start body-start end target-buffer target-char link block-name body)
    (save-window-excursion
      (save-excursion
    (while (and (re-search-backward org-link-bracket-re nil t)
            (not ; ever wider searches until matching block comments
             (and (setq start (line-beginning-position))
              (setq body-start (line-beginning-position 2))
              (setq link (match-string 0))
              (setq block-name (match-string 2))
              (save-excursion
                (save-match-data
                  (re-search-forward
                   (concat " " (regexp-quote block-name)
                       " ends here")
                   nil t)
                  (setq end (line-beginning-position))))))))
	(unless (and start (< start mid) (< mid end))
	  (error "Not in tangled code"))
        (setq body (buffer-substring body-start end)))
      ;; Go to the beginning of the relative block in Org file.
      (org-link-open-from-string link)
      (message "%s" link)
      (setq target-buffer (current-buffer))
      ;; (search-forward body)
      (if (string-match "[^ \t\n\r]:\\([[:digit:]]+\\)" block-name)
          (let ((n (string-to-number (match-string 1 block-name))))
            (if (org-before-first-heading-p) (goto-char (point-min))
              (org-back-to-heading t))
            ;; Do not skip the first block if it begins at point min.
            (cond ((or (org-at-heading-p)
                       (not (eq (org-element-type (org-element-at-point))
                		'src-block)))
                   (org-babel-next-src-block n))
                  ((= n 1))
                  (t (org-babel-next-src-block (1- n)))))
        (org-babel-goto-named-src-block block-name))
      (goto-char (org-babel-where-is-src-block-head))
      (forward-line 1)
      ;; Try to preserve location of point within the source code in
      ;; tangled code file.
      (let ((offset (- mid body-start)))
        (when (< end (+ offset (point))) ; ybyygu hacked here
          (forward-char offset)))
      (setq target-char (point)))
    (org-src-switch-to-buffer target-buffer t)
    (goto-char target-char)))
;; 9b40c7cf ends here

;; [[file:../../../gwp.note::f1b57cf1][f1b57cf1]]
;; tangle blocks for current file at point
;; http://stackoverflow.com/questions/28727190/org-babel-tangle-only-one-code-block
;; call org-babel-tangle with C-u C-u
(defun gwp/org-babel-tangle-blocks()
  (interactive)
  ;; tangle blocks only for target file at point
  (let ((current-prefix-arg '(16)))     ; C-u C-u
    (call-interactively 'org-babel-tangle)))

;; narrow to subtree before calling org-babel-tangle
(defun gwp/org-tangle-subtree()
  "Tange src blocks in current subtree"
  (interactive)
  (org-narrow-to-subtree)
  (org-babel-tangle)
  (widen)
  )
;; f1b57cf1 ends here

;; [[file:../../../gwp.note::566a6ed9][566a6ed9]]
(defun gwp/org-edit-save-and-tangle ()
  "When in a sub-editing buffer, swith to the parent buffer and tangle the file blocks"
  (interactive)
  (save-excursion
    (org-edit-src-exit)
    ;; insert an unique code block name
    (gwp/org-src-insert-name)
    (call-interactively 'gwp/org-babel-tangle-blocks)
    (org-edit-src-code)))

(defun gwp/org-babel-tangle-dwim()
  "Tangle current file at point whenever in a sub-editing buffer or not"
  (interactive)
  ;; 标记当前位置
  (gwp::mark-current-position)
  (if (org-src-edit-buffer-p)
      (gwp/org-edit-save-and-tangle)
    (if (eq 'src-block (org-element-type (org-element-at-point)))
        (progn
          ;; insert an unique code block name
          (gwp/org-src-insert-name)
          (call-interactively 'gwp/org-babel-tangle-blocks))
      (message "not in source block"))))
;; 566a6ed9 ends here

;; [[file:../../../gwp.note::661f0512][661f0512]]
(defun gwp/org-babel-tangle-no()
  "Turn on or turn off tangling current code block"
  (interactive)
  (if (eq 'src-block (org-element-type (org-element-at-point)))
      (save-excursion
        (org-babel-goto-src-block-head)
        (if (re-search-forward ":tangle no" (line-end-position) t)
            (delete-region (match-beginning 0) (match-end 0))
          (org-babel-insert-header-arg "tangle" "no")))
    (org-set-property "header-args" ":tangle no")))
;; 661f0512 ends here

;; [[file:../../../gwp.note::1a4b128e][1a4b128e]]
(defun gwp/org-src-insert-name ()
  "If it doesn't have a NAME property then assign it an unique name."
  (interactive)
  (let ((element (org-element-at-point)))
    (if (eq 'src-block (org-element-type element))
        (if (not (org-element-property :name element))
            (save-excursion
              (goto-char (org-babel-where-is-src-block-head))
              (let ((i (current-indentation)))
                (save-excursion (insert "#+name: " (substring (org-id-new) 0 8) "\n"))
                (indent-to i)))
          (message "source block alread named"))
      (message "not in source block"))))
;; 1a4b128e ends here

;; [[file:../../../gwp.note::e9fca5dc][e9fca5dc]]
(with-eval-after-load 'ob
  (setq org-structure-template-alist
        '(
          ("py" . "src python :results output")
          ("rs" . "src rust")
          ("el" . "src emacs-lisp")
          ("sh" . "src sh")
          ))

  (defun gwp/org-babel-edit-structure-in-place (arg)
    "Insert source strcture and edit the source"
    (interactive "P")
    (call-interactively 'org-insert-structure-template)
    (call-interactively 'org-edit-src-code)
    ))
;; e9fca5dc ends here

;; [[file:../../../gwp.note::*auto time-stamp][auto time-stamp:1]]
(with-eval-after-load "ob-tangle"
  ;; update timestamps on tangled files
  (setq time-stamp-pattern "100/UPDATED:[ \t]+\\\\?[\"<]+%:y-%02m-%02d %3a %02H:%02M\\\\?[\">]")
  (defun org-babel-post-tangle-hook--time-stamp ()
    "Update timestamps on tangled files."
    (time-stamp)
    (save-buffer))
  (add-hook 'org-babel-post-tangle-hook 'org-babel-post-tangle-hook--time-stamp))
;; auto time-stamp:1 ends here

;; [[file:../../../gwp.note::21ae7ae2][21ae7ae2]]
;; 默认的不太好按. 不能用C-c C-c, 容易与别的模块冲突.
(map! :map org-src-mode-map
      "C-c ;"   #'org-edit-src-exit  ; 保存退出
      "C-c C-;" #'org-edit-src-exit  ; 保存退出
      "C-c C-k" #'org-edit-src-abort ; 放弃修改
      )

(gwp::local-leader-def
  :keymaps 'org-src-mode-map
  "q" #'org-edit-src-exit
  )

(general-define-key :prefix-map 'gwp::org-babel-map)
;; 更多的命令定义在org-babel-map
(map! :map gwp::org-babel-map
      :desc "check src block headers"    "c" #'org-babel-check-src-block
      :desc "insert header argument"     "i" #'org-babel-insert-header-arg
      :desc "view header arguments"      "I" #'org-babel-view-src-block-info
      :desc "demarcate block"            "d" #'org-babel-demarcate-block
      :desc "edit src codes in place"    "s" #'gwp/org-babel-edit-structure-in-place
      :desc "jump to tangled file"       "j" #'gwp/org-babel-tangle-jump-to-file
      :desc "insert header tangle no"    "n" #'gwp/org-babel-tangle-no
      :desc "execute in edit buffer"     "x" #'org-babel-do-key-sequence-in-edit-buffer
      :desc "tangle blocks in subtree"   "t" #'gwp/org-tangle-subtree
      :desc "name code block at point"   "SPC" #'gwp/org-src-insert-name
      :desc "tangle blocks in buffer"    "T" #'org-babel-tangle
      :desc "switch org buffer"          "b" #'org-switchb ; 仿SPC-b-b
      )

(gwp::local-leader-def
  :keymaps 'org-mode-map
  "M-p" '(org-previous-block :which-key "previous block")
  "M-n" '(org-next-block :which-key "next block")

  ;; 进入代码编辑模式, 改成容易按的
  ";" '(org-edit-special :which-key "edit source code")
  "C-c ;" '(org-edit-special :which-key "edit source code")
  "C-c C-;" '(org-edit-special :which-key "edit source code")

  "b" '(:keymap gwp::org-babel-map :which-key "babel/buffer")
  )
;; 21ae7ae2 ends here

;; [[file:../../../gwp.note::ebc6075d][ebc6075d]]
(defun gwp::org-toggle-checkbox ()
  (interactive)
  (unless (org-at-item-p)
    (call-interactively #'org-toggle-item)
    )
  (let ((current-prefix-arg '(4)))     ; C-u
    (call-interactively #'org-toggle-checkbox)))
;; ebc6075d ends here

;; [[file:../../../gwp.note::1e605e7a][1e605e7a]]
(general-define-key :prefix-map 'gwp::org-toggle-map)

(map! :map gwp::org-toggle-map
      :desc "preview inline images"
      "I" #'org-toggle-inline-images
      :desc "preview latex fragments"
      "L" #'org-latex-preview
      :desc "Paste image from clipboard"
      "C-v" #'org-download-clipboard
      :desc "change the TODO state"
      "t" #'org-todo
      :desc "heading"
      "h" #'org-toggle-heading
      :desc "item"
      "i" #'org-toggle-item
      :desc "tag"
      "s" #'counsel-org-tag
      :desc "fixed-width markup (:)"
      ":" #'org-toggle-fixed-width
      :desc "toggle checkbox"
      "c" #'gwp::org-toggle-checkbox)

(gwp::local-leader-def
 :keymaps 'org-mode-map
 "-" '(org-ctrl-c-minus :which-key "toggle item (-)")
 "*" '(org-ctrl-c-star :which-key "toggle headline (*)")

 "t" '(:keymap gwp::org-toggle-map :which-key "toggle")
 )
;; 1e605e7a ends here

;; [[file:../../../gwp.note::27b50206][27b50206]]
(defun gwp/org-delete-link-file (arg)
  "Delete the file that link points to."
  (interactive "P")

  (save-excursion
    (org-next-link)
    (let ((file (gwp/org-file-path-at-point)))
      (if file
          (if (file-exists-p file)
              (when (yes-or-no-p (format "Delete link file: %s?" file))
                (progn (delete-file file)
                       (message "File deleted")))
            (error "No such attachment: %s" file))
        (user-error "Point is not on a file link")))))
;; 27b50206 ends here

;; [[file:../../../gwp.note::*latex preview][latex preview:1]]
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5))
;; latex preview:1 ends here

;; [[file:../../../gwp.note::7330d8ac][7330d8ac]]
;; (setq browse-url-browser-function 'browse-url-firefox)

;; If available, use `xdg-open' to open URLs.
(setq-default
 browse-url-browser-function (quote browse-url-generic)
 browse-url-generic-program "xdg-open")
;; 7330d8ac ends here

;; [[file:../../../gwp.note::32a3b56a][32a3b56a]]
(general-define-key :prefix-map 'gwp::org-link-map)

(map! :map gwp::org-link-map
      "l" #'org-insert-link
      "r" #'org-download-rename-at-point
      "D" #'gwp/org-delete-link-file
      )

(gwp::local-leader-def
 :keymaps 'org-mode-map
 [tab] '(org-next-link :which-key "goto next link")
 [backtab] '(org-previous-link :which-key "goto next link")

 "l" '(:keymap gwp::org-link-map :which-key "link")
 )
;; 32a3b56a ends here

;; [[file:../../../gwp.note::4971b464][4971b464]]
;;;###autoload
(defun gwp/search-all-notes (&optional arg)
  "search all notes in ~/.cache/notes"
  (interactive)
  ;; (defun counsel-rg (&optional initial-input initial-directory extra-rg-args rg-prompt)
  (let ((counsel-rg-base-command (list
                                  "ripgrep"
                                  "-M" "240"
                                  "--with-filename"
                                  "--no-heading"
                                  "--line-number"
                                  "--color" "never"
                                  "%s")))
    (if arg
        (counsel-rg arg "~/.cache/notes")
      (counsel-rg "" "~/.cache/notes"))))
;; 4971b464 ends here

;; [[file:../../../gwp.note::05419467][05419467]]
;;;###autoload
(defun gwp/org-notes-search (query)
  "Perform a text search on `org-directory'."
  (interactive
   (list (if (doom-region-active-p)
             (buffer-substring-no-properties
              (doom-region-beginning)
              (doom-region-end))
           "")))
  (require 'org)
  (counsel-rg query org-directory))

(defun gwp/find-file-in-notes ()
  "Find a file under `~/.cache/notes', recursively."
  (interactive) (doom-project-find-file "~/.cache/notes"))
;; 05419467 ends here

;; [[file:../../../gwp.note::37fef008][37fef008]]
(use-package! org-noter
  :custom
  (org-noter-default-notes-file-names '("annotation.note")))

(use-package! pdf-tools
  ;; :hook (pdf-tools-enabled . pdf-view-themed-minor-mode)
  ;; 自动切边
  :hook (pdf-tools-enabled . pdf-view-auto-slice-minor-mode)
  :hook (pdf-tools-enabled . hide-mode-line-mode)
  :custom
  ;; 一页页看更方便
  (pdf-view-continuous nil)
  :config
  ;; 容易被doom的pdf module中的设置覆盖, 以下直接在pdf/config.el中修改
  ;; (setq-default pdf-view-display-size 'fit-width)
  (map! :map pdf-view-mode-map
        ;; 鼠标操作
        [C-mouse-5] (cmd! (pdf-view-shrink 1.10))
        [C-mouse-5] (cmd! (pdf-view-shrink 1.10))
        [mouse-9] (cmd! (pdf-view-scroll-down-or-previous-page))
        [mouse-8] (cmd! (pdf-view-scroll-up-or-next-page))
        ;; 方便标注, 按d直接高亮选中文本
        "d" #'pdf-annot-add-highlight-markup-annotation
        ;; 方便单手操作
        "n" #'pdf-view-scroll-up-or-next-page
        "p" #'pdf-view-scroll-down-or-previous-page
        ;; 默认按键, 设置下为保险
        "W" #'pdf-view-fit-width-to-window
        "B" #'pdf-history-backward
        ))
;; 37fef008 ends here

;; [[file:../../../gwp.note::*capture][capture:1]]
(setq org-capture-templates
      '(
        ("n" "Note" entry (file "~/Notes/refile.note")
         "* %u %? %(org-get-x-clipboard 'CLIPBOARD)\n  %:initial\n" :prepend t)
        ("t" "Task" entry (file+headline "~/Notes/life.note" "Tasks")
         "* TODO %^T %? %(org-get-x-clipboard 'CLIPBOARD)\n  %i" :prepend t)
        ("r" "Research Memo" entry (file+headline "~/Notes/research.note" "Memo")
         "* %u %? %(org-get-x-clipboard 'CLIPBOARD)\n  %i\n" :prepend t)
        ("p" "Paper" entry (file+headline "~/Notes/research.note" "Literature")
         "* TODO %u %? %(org-get-x-clipboard 'CLIPBOARD)\n  %i\n" :prepend t)
        ("j" "Life Journal" entry (file+headline "~/Notes/life.note" "Journals")
         "* %u %? %(org-get-x-clipboard 'CLIPBOARD)\n  %i\n" :prepend t)
        ("N" "Note from protocol" entry (file "~/Notes/refile.note")
         "* %u %? [[%:link][%:description]]\n  %:initial\n" :prepend t)))
;; capture:1 ends here

;; [[file:../../../gwp.note::cf2d1a6b][cf2d1a6b]]
(require 'init-note)
;; cf2d1a6b ends here

;; [[file:../../../gwp.note::f48c192c][f48c192c]]
(require 'init-eaf)
;; f48c192c ends here

;; [[file:../../../gwp.note::ac0d3d18][ac0d3d18]]
(general-define-key :prefix-map 'gwp::org-note-map)

(map! :map gwp::org-note-map
      "o" #'gwp::org-note::open-pdf
      "i" #'gwp::org-note::new-note
      "b" #'gwp::org-backlinks
      "s" #'org-sidebar-tree-toggle
      "n" #'gwp::org-note::dired-annotate-file-at-point
      )

;; org-noter很好用
(gwp::local-leader-def
  :keymaps 'pdf-view-mode-map
  "n" #'org-noter
  "i" #'org-noter-insert-note
  "I" #'org-noter-insert-precise-note
  )

(gwp::local-leader-def
  :keymaps 'org-mode-map
  "n" '(:keymap gwp::org-note-map :which-key "note/noter")
  )
;; ac0d3d18 ends here

;; [[file:../../../gwp.note::da4e0834][da4e0834]]
(defun gwp::update-notes-cache ()
  (interactive)
  (message (shell-command-to-string "rebuild-note-cache.sh")))

(require 'midnight)
(midnight-mode t)
(midnight-delay-set 'midnight-delay "3:30am")

(add-hook! midnight #'gwp::update-notes-cache)
;; da4e0834 ends here

;; [[file:../../../gwp.note::d737c18e][d737c18e]]
;; (setq org-export-backends '(ascii html icalendar latex odt md))
(require 'ox-md)
;; d737c18e ends here

;; [[file:../../../gwp.note::*zotero/export][zotero/export:1]]
(with-eval-after-load 'org-compat
  (defun gwp/org-zotero-export (path desc format)
    "Create the export version of zotero link specified by PATH and
DESC. FORMATs understood are 'odt','latex and 'html."
    (cond
     ((eq format 'html)
      (format "<a href=\"zotero:%s\">%s</a>" path desc))
     ((eq format 'latex)
      (format "\\href{zotero:%s}{%s}" path desc))
     ((eq format 'odt)
      ;; (format "<text:a xlink:type=\"simple\" xlink:href=\"zotero:%s\">%s</text:a>" path desc)
      (gwp/org-zotero-export-odt path desc)
      )
     (t desc)
     )
    )
  )

;;;; The magic string of zitem:
;; ZOTERO_ITEM CSL_CITATION
;; {
;; "properties": {
;; "formattedCitation": "[1]",
;; "plainCitation": "[1]"
;; },
;; "citationItems": [
;;                   {
;;                   "uri": [
;;                           "http://zotero.org/users/15074/items/S5JM4V35"
;;                           ]
;;                   }
;;                   ],
;; "schema": "https://github.com/citation-style-language/schema/raw/master/csl-citation.json"
;; } %s-rnd

;; adopted from https://www.mail-archive.com/emacs-orgmode@gnu.org/msg48905.html
(defun gwp/org-zotero-export-odt (path desc)
  (let
      ((refmark "<text:reference-mark-start text:name=\"%s\"/>%s<text:reference-mark-end text:name=\"%s\"/>")
       (zitem "ZOTERO_ITEM CSL_CITATION {
    &quot;properties&quot;: {
        &quot;formattedCitation&quot;: &quot;%s&quot;,
        &quot;plainCitation&quot;: &quot;%s&quot;
    },
    &quot;citationItems&quot;: [
        {
            &quot;uri&quot;: [
                &quot;http://zotero.org/users/15074/items/%s&quot;
            ]
        }
    ],
    &quot;schema&quot;: &quot;https://github.com/citation-style-language/schema/raw/master/csl-citation.json&quot;
} %s ")

       (item-key (car (cdr (split-string path "_"))))
       (rnd (concat "RND" (substring (org-id-new) -10))))
    (setq zitem
          (format zitem
                  desc
                  desc
                  item-key
                  rnd)
          )
    (setq desc (format "%s" desc))
    (format refmark zitem desc zitem))
  )
;; zotero/export:1 ends here

;; [[file:../../../gwp.note::8fc79737][8fc79737]]
(use-package ox-odt
  :config
  (progn
    ;; continually numbering captions without outline level
    (setq org-odt-display-outline-level 0)

    ;; useful for odt export using dvipng
    (setq org-format-latex-options (plist-put org-format-latex-options :html-scale 3.0))
    (setq org-odt-pixels-per-inch 300.0)))
;; 8fc79737 ends here

;; [[file:../../../gwp.note::*odt export][odt export:2]]
;; adopted from https://github.com/tumashu/emacs-helper/blob/master/eh-org.el
(defun gwp/clear-unwanted-space (text)
  "clear unwanted space when exporting org-mode to other formats"
  (let ((regexp "[[:multibyte:]]")
        (string text))
    ;; org-mode 默认将一个换行符转换为空格，但中文不需要这个空格，删除。
    (setq string
          (replace-regexp-in-string
           (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
           "\\1\\2" string))
    ;; 删除粗体之后的空格
    (dolist (str '("</b>" "</code>" "</del>" "</i>"))
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\)\\(%s\\)[ ]+\\(%s\\)" regexp str regexp)
             "\\1\\2\\3" string)))
    ;; 删除粗体之前的空格
    (dolist (str '("<b>" "<code>" "<del>" "<i>" "<span class=\"underline\">"))
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\)[ ]+\\(%s\\)\\(%s\\)" regexp str regexp)
             "\\1\\2\\3" string)))
    string)
  )

(defun gwp/ox-odt-wash-text (text backend info)
  "导出 org file 时，删除中文之间不必要的空格。"
  (when (org-export-derived-backend-p backend 'odt 'html 'latex)
    (gwp/clear-unwanted-space text)
    )
  )

(add-hook 'org-export-filter-headline-functions #'gwp/ox-odt-wash-text)
(add-hook 'org-export-filter-paragraph-functions #'gwp/ox-odt-wash-text)
;; odt export:2 ends here

;; [[file:../../../gwp.note::d7c4714d][d7c4714d]]
(general-define-key :prefix-map 'gwp::org-export-map)

(map! :map gwp::org-export-map
      :desc "export dispatch" "e" #'org-export-dispatch
      )

(gwp::local-leader-def
 :keymaps 'org-mode-map
 "e" '(:keymap gwp::org-export-map :which-key "export")
 )
;; d7c4714d ends here

;; [[file:../../../gwp.note::*setup][setup:1]]
(require 'org-attach)
;; setup:1 ends here

;; [[file:../../../gwp.note::aae629f1][aae629f1]]
(setq org-attach-store-link-p 'attached)
;; 子节点可使用父节点的 attach/ID 目录
(setq org-attach-use-inheritance t)

;; 1. store the ataach files into clipboard
(defun gwp/org-attach-copy (&optional force)
  "store org attachment directory of current enetry"
  (interactive "P")
  ;; make a temporary symlink to store the attachment path
  (if-let (attach-files (counsel-org-files))
      (let ((current-dir (if buffer-file-name (file-name-directory buffer-file-name) default-directory)))
        (zotero-attach-txclip-copy-files attach-files current-dir))
    (message "No attachment found")))

;; 2. paste the stored files to new location
(defun gwp/org-attach-paste (&optional force)
  "move stored attachments to current entry"
  (interactive "P")
  (let ((current-dir (file-name-directory buffer-file-name)))
    (zotero-attach-txclip-paste-files current-dir)))
;; aae629f1 ends here

;; [[file:../../../gwp.note::b47bc445][b47bc445]]
(defun gwp/org-file-link-p (&optional element)
  (let ((el (or element (org-element-context))))
    (and (eq (org-element-type el) 'link)
         (or
          (string= (org-element-property :type el) "file")
          (string= (org-element-property :type el) "attachment")))))

(defun gwp/org-file-path-at-point()
  "get file path from link at point"
  (let ((el (org-element-context)))
    (when (eq (org-element-type el) 'link)
      (cond
       ((string= (org-element-property :type el) "file") (org-element-property :path el))
       ((string= (org-element-property :type el) "attachment") (org-attach-expand (org-element-property :path el)))
       (t nil)))))
;; b47bc445 ends here

;; [[file:../../../gwp.note::*使用org-attach将文件move到当到附录中并更新文件链接][使用org-attach将文件move到当到附录中并更新文件链接:1]]
;; (require 'org-download)

(defun gwp/org-store-link-without-desc (file)
  "store file link without the description part -- a tweak to make odt image exporting correct."
  (setq org-stored-links
        (cons (list (org-attach-expand-link (file-name-nondirectory file)) "")
              org-stored-links)))

(defun gwp/org-take-as-local-attachment ()
  "move file link at point as local attachment"
  (interactive)
  (let ((file (gwp/org-file-path-at-point)))
    (if file
        (progn
          ;; 1. store the file using copy
          ;; or we can use the mv method: (org-attach-attach file nil 'mv)
          ;; do not store file link since it will corrupt odt image exporting
          (let ((org-attach-store-link-p nil))
            (org-attach-attach file))
          ;; 2. remove the old
          (call-interactively 'org-download-delete)
          ;; 3. insert the new
          ;; use file name as the default caption
          (gwp/org-insert-image-attributes (file-name-sans-extension (file-name-nondirectory file)))
          (insert "\n")
          (gwp/org-store-link-without-desc file)
          (call-interactively 'org-insert-last-stored-link)
          ;; refresh the image if possbile
          (org-display-inline-images))
      (user-error "Point is not on a link"))))
;; 使用org-attach将文件move到当到附录中并更新文件链接:1 ends here

;; [[file:../../../gwp.note::55d587f4][55d587f4]]
(defun gwp/org-attach-auto-directory ()
  "为当前 headline 设置 DIR 属性 (基于 ID)"

  (interactive)
  (let* ((attach-dir (org-attach-dir-from-id (org-id-new)))
         (current-dir (file-name-directory (or default-directory
                                               buffer-file-name)))
         (attach-dir-relative (file-relative-name attach-dir current-dir)))
    (org-entry-put nil "DIR" attach-dir-relative)
    attach-dir))
;; 55d587f4 ends here

;; [[file:../../../gwp.note::43fd72e2][43fd72e2]]
(with-eval-after-load 'org-agenda
  ;; 2013-01-20: less is more
  ;; (setq org-agenda-files (append (file-expand-wildcards "~/Notes/*.note") (file-expand-wildcards "~/Notes/*/*.note")))
  (setq org-agenda-files "~/Notes/.agenda_files")

  ;; the default is todo-start
  (setq org-icalendar-use-scheduled (quote (event-if-not-todo event-if-todo todo-start)))
  (setq org-icalendar-alarm-time 5)

  ;; Show all future entries for repeating tasks
  (setq org-agenda-repeating-timestamp-show-all t)

  ;; do not show agenda dates if they are empty
  (setq org-agenda-show-all-dates nil)

  ;; Sorting order for tasks on the agenda
  (setq org-agenda-sorting-strategy
        (quote ((agenda time-up priority-down category-up)
                (todo priority-down)
                (tags priority-down))))

  ;; Start the weekly agenda today
  (setq org-agenda-start-on-weekday nil)

  ;; do not include todo items
  (setq org-agenda-include-all-todo nil))
;; 43fd72e2 ends here

;; [[file:../../../gwp.note::*agenda][agenda:2]]
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
;; agenda:2 ends here

;; [[file:../../../gwp.note::*agenda][agenda:3]]
(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
               '(
                 ("g" . "GTD contexts") ; description for "g" prefix
                 )
               )
  ;; project overview
  (add-to-list 'org-agenda-custom-commands
               '("gp" "Project"
                 (
                  (tags "Project+Action+TODO=\"TODO\""
                        (
                         (org-agenda-overriding-header "Project\n------------------")
                         (org-agenda-sorting-strategy '(priority-down category-keep timestamp-up))
                         )
                        )
                  (tags "Action+Study+TODO=\"TODO\""
                        (
                         (org-agenda-overriding-header "Topics\n------------------")
                         (org-agenda-files '("~/Notes/research.note"))
                         (org-agenda-sorting-strategy '(priority-down timestamp-up))
                         (org-agenda-max-entries 5)
                         )
                        )
                  (tags "Action+TODO=\"TODO\""
                        (
                         (org-agenda-overriding-header "生活琐事\n------------------")
                         (org-agenda-files '("~/Notes/life.note"))
                         (org-agenda-sorting-strategy '(priority-down timestamp-up))
                         (org-agenda-max-entries 5)
                         )
                        )
                  ;; (tags "Computer+TODO=\"TODO\""
                  ;;       (
                  ;;        (org-agenda-overriding-header "电脑调优\n------------------")
                  ;;        (org-agenda-sorting-strategy '(priority-down timestamp-up))
                  ;;        (org-agenda-max-entries 5)
                  ;;        )
                  ;;       )
                  )
                 ;; options set here apply to the entire block
                 (
                  (org-tags-match-list-sublevels nil)
                  (org-agenda-prefix-format "%-20c ")
                  (org-agenda-todo-keyword-format "")
                  (org-agenda-remove-tags t)
                  (org-agenda-compact-blocks t)
                  )
                 )
               )

  (add-to-list 'org-agenda-custom-commands
               '("gr" "Reading"
                 (
                  (tags-todo "Reading|Read"
                             (
                              (org-agenda-overriding-header "待读列表\n------------------")
                              (org-agenda-sorting-strategy '(category-keep priority-down))
                              (org-agenda-remove-tags t)
                              (org-agenda-compact-blocks t)
                              )
                             )
                  (tags "REFILE"
                        (
                         (org-agenda-overriding-header "Tasks to Refile\n------------------")
                         (org-tags-match-list-sublevels nil)
                         )
                        )
                  )
                 ;; options set here apply to the entire block
                 ((org-agenda-compact-blocks t))
                 )
               )

  (add-to-list 'org-agenda-custom-commands
               '("gt" "Tasks"
                 (
                  (agenda ""
                          (
                           ;; (org-agenda-entry-types '(:deadline :scheduled))
                           (org-agenda-span (quote month)) ;; or (org-agenda-span 90)
                           (org-agenda-include-diary nil)
                           (org-agenda-overriding-header "Agenda\n------------------")
                           )
                          )
                  ;; (tags "ASAP+TODO=\"TODO\""
                  (tags-todo "ASAP"
                        (
                         (org-agenda-entry-types '(:timestamp))
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                         (org-agenda-overriding-header "\nASAP\n------------------")
                         (org-agenda-sorting-strategy '(priority-down category-keep timestamp-up))
                         (org-agenda-max-entries 20)
                         (org-agenda-prefix-format "%-12c ")
                         (org-agenda-compact-blocks t)
                         )
                        )
                  )
                 ;; options set here apply to the entire block
                 (
                  (org-tags-match-list-sublevels nil)
                  ;; (org-agenda-files '("~/Notes/research.note" "~/Notes/life.note"))
                  (org-agenda-todo-keyword-format "")
                  (org-agenda-remove-tags t)
                  )
                 ;; agenda view exported with: Ctrl-C a e
                 ("~/Notes/agenda.html" "~/Notes/agenda.txt")
                 )
               )
  )
;; agenda:3 ends here

;; [[file:../../../gwp.note::c09b236a][c09b236a]]
(general-define-key :prefix-map 'gwp::org-attach-map)

(map! :map gwp::org-attach-map
       :desc "attachment" "a" #'org-attach
       :desc "set attach directory" "n" #'gwp/org-attach-auto-directory
      )

(gwp::local-leader-def
 :keymaps 'org-mode-map
 "a" '(:keymap gwp::org-attach-map :which-key "attach/agenda")
 )
;; c09b236a ends here

;; [[file:../../../gwp.note::ac1d0086][ac1d0086]]
(use-package org-id
  :custom
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))
;; ac1d0086 ends here

;; [[file:../../../gwp.note::98fd4d7a][98fd4d7a]]
(require 'el-patch)
(defun gwp::org-id-find-id-file (id)
  (let* ((rg-command (format "ripgrep -l --color never -e '^\\s*:ID:\\s+%s' /home/ybyygu/.cache/notes" id))
         (output (shell-command-to-string rg-command))
         (file (car (split-string output "[\r\n]+" t))))
    ;; (message "%s" rg-command)
    file))

(el-patch-feature org-id)
(with-eval-after-load 'org-id
  (el-patch-defun org-id-find-id-file (id)
    "Query the id database for the file in which ID is located."
    (unless org-id-locations (org-id-locations-load))
    (or (and org-id-locations
             (hash-table-p org-id-locations)
             (gethash id org-id-locations))
        ;; Fall back on current buffer
        (or
         (gwp::org-id-find-id-file id)
         (buffer-file-name (or (buffer-base-buffer (current-buffer))
                               (current-buffer)))))))
;; 98fd4d7a ends here

;; [[file:../../../gwp.note::a4f269ca][a4f269ca]]
(require 'org-sidebar)

(use-package org-sidebar
  :custom
  (org-sidebar-side 'left)                                                         ; 新版中 left 是默认
  (org-ql-sidebar-buffer-setup-hook nil)                                           ; 避免多行显示, 太乱
  (org-sidebar-default-fns '(gwp::org-sidebar--backlinks org-sidebar--todo-items)) ; 使用反链视图
  (org-sidebar-tree-jump-fn 'org-sidebar-tree-jump-source)                         ; 跳至源文件对应的位置, 而不是 narrowed heding
  :config
  ;; 避免误按
  (map! :map org-sidebar-tree-map
        [mouse-1] nil
        [drag-mouse-1] nil
        ))

;;;###autoload
(defun gwp::org-backlinks ()
  "显示指向当前 heading 的反向链接"
  (interactive)

  (let* ((org-sidebar-side 'right)
         (id (org-entry-get (point) "ID"))
         (custom-id (org-entry-get (point) "CUSTOM_ID"))
         (query (gwp::org-backlinks--get-query id custom-id)))
    (org-sidebar-ql (gwp::org-backlinks-search-files id)
      query :title (concat "Links to: " (org-get-heading t t)))))

(defun gwp::org-backlinks--get-query (id custom-id)
  (cond ((and id custom-id)
         ;; This will be slow because it isn't optimized to a single regexp.  :(
         (warn "Entry has both ID and CUSTOM_ID set; query will be slow")
         `(or (link :target ,(concat "id:" id))
              (link :target ,(concat "id:" custom-id))))
        ((or id custom-id)
         `(link :target ,(concat "id:" (or id custom-id))))
        (t (error "Entry has no ID nor CUSTOM_ID property"))))


;; reference:
;; (collection (funcall ffip-project-search-function cmd))
(defun gwp::org-backlinks-search-files (keyword)
  "搜索文件系统中所有的.note文件, 返回包含引用 keyword 的文件名"
  (let* (
         (rg-command (format "ripgrep -l --color never -e %s /home/ybyygu/.cache/notes" keyword))
         (output (shell-command-to-string rg-command))
         (collection (split-string output "[\r\n]+" t))
         result)
    ;; (message "shell output:\n%s\nshell output ends here" output)
    (dolist (file collection result) (push file result))
    result))

(defun gwp::org-sidebar--backlinks (source-buffer)
  "在 org-sidebar 中显示 backlinks buffer"
  (let* ((display-buffer (generate-new-buffer (format "org-sidebar<%s>" (buffer-name source-buffer))))
         (title (propertize (concat "反链条目: " (buffer-name source-buffer)) 'help-echo "含有指向当前heading链接的条目"))
         (id (org-entry-get (point) "ID"))
         (custom-id (org-entry-get (point) "CUSTOM_ID"))
         (source-buffers (gwp::org-backlinks-search-files id))
         query)
    (with-current-buffer display-buffer
      (setf org-sidebar-source-buffer source-buffer))

    ;; 如果当前 heading 无 ID, 不报错
    (condition-case err
        (setq query (gwp::org-backlinks--get-query id custom-id))
      (error
       (message "%s" (error-message-string err))
       (setq source-buffers nil)))
    (org-ql-search source-buffers
      query
      :buffer display-buffer
      :title title)

    display-buffer))
;; a4f269ca ends here

;; [[file:../../../gwp.note::aa6a42b8][aa6a42b8]]
(general-define-key :prefix-map 'gwp::org-goto-map)

(map! :map gwp::org-goto-map
      :desc "previous position"
      "p" #'org-mark-ring-goto
      :desc "标记位置"
      "m" #'org-mark-ring-push
      :desc "Jump to org heading"
      "g" #'counsel-org-goto
      :desc "跳转至代码块"
      "b"   #'gwp/org-jump-block/body
      :desc "跳转至链接"
      "l"   #'gwp/org-jump-link/body
      :desc "跳转至 tangled 代码文件"
      "t"   #'gwp/org-babel-tangle-jump-to-file
      )

(gwp::local-leader-def
 :keymaps 'org-mode-map
 "g" '(:keymap gwp::org-goto-map :which-key "goto/jump")
 )
;; aa6a42b8 ends here

;; [[file:../../../gwp.note::fbbec921][fbbec921]]
;; 取自doom org moudle
(defun gwp::org-dwim-at-point (&optional arg)
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- clock: update its time.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: edit org-src
- statistics-cookie: update it.
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-property :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        (`babel-call
         (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ;; Hacked by ybyygu at 2021-04-13
        ((or `src-block `inline-src-block)
         (org-edit-special arg))

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (org-toggle-inline-images)
             ;; 强制在本窗口打开
             (let ((current-prefix-arg '(16)))     ; C-u C-u
               (call-interactively #'gwp::org-open-at-point-dwim)))))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
           (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           ;; (+org--toggle-inline-images-in-subtree
           ;;  (org-element-property :begin context)
           ;;  (org-element-property :end context))
           ))))))
;; fbbec921 ends here

;; [[file:../../../gwp.note::2f61258f][2f61258f]]
;; https://stackoverflow.com/questions/17590784/how-to-let-org-mode-open-a-link-like-file-file-org-in-current-window-inste
;; Depending on universal argument try opening link
(defun gwp::org-open-at-point-dwim (&optional arg)
  (interactive "P")
  (cond
   ((equal arg '(16))                    ; C-u C-u
    (let ((org-link-frame-setup (quote ((file . find-file)))))
      (org-open-at-point)))
   ((equal arg '(4))                     ; C-u
    (let ((org-link-frame-setup (quote ((file . find-file-other-frame)))))
      (org-open-at-point)))
   (t                                   ; the default behavior
    (let ((org-link-frame-setup (quote ((file . find-file-other-window)))))
      (org-open-at-point)
      (golden-ratio)))))

;; 注释代码时, 在org code block下特殊处理. 不然光标会跳开很远.
(defun gwp/comment-or-uncomment-dwim ()
  (interactive)
  (save-excursion
    (if (org-in-src-block-p)
        (progn
          (org-edit-src-code)
          (call-interactively 'comment-dwim)
          (org-edit-src-exit))
      (call-interactively 'comment-dwim))))
;; 2f61258f ends here

;; [[file:../../../gwp.note::c99c78d1][c99c78d1]]
;; 避免与 org-sidebar 的设置冲突
(map! :map org-sidebar-tree-map
      [return] nil)

(map! :map org-mode-map
      "C-c C-o" #'gwp::org-open-at-point-dwim)

(gwp::local-leader-def
  :keymaps 'org-mode-map
  "o"      #'gwp::org-open-at-point-dwim
  "RET"    #'gwp::org-dwim-at-point
  [return] #'gwp::org-dwim-at-point
  )

(gwp::dwim-leader-def
  :keymaps 'org-mode-map
  "g" 'counsel-org-goto                        ; goto
  "t" 'org-todo                                ; todo
  "b" 'gwp/org-babel-tangle-dwim               ; babel
  "e" 'org-edit-special                        ; edit
  "a" 'org-attach                              ; attach
  "n" 'gwp::org-babel-narrow-to-tangle-heading ; narrow
  )

(gwp::dwim-leader-def
  :keymaps 'org-src-mode-map
  "b" 'gwp/org-babel-tangle-dwim
  "q" 'org-edit-src-exit
  )
;; c99c78d1 ends here

;; [[file:../../../gwp.note::aa6a42b8][aa6a42b8]]
(general-define-key :prefix-map 'gwp::org-mark-map)

(map! :map gwp::org-mark-map
      "." #'org-mark-element
      "m" #'org-mark-ring-push
      "p" #'org-mark-ring-goto
      "s" #'org-babel-mark-block
      )

(gwp::local-leader-def
 :keymaps 'org-mode-map
 "m" '(:keymap gwp::org-mark-map :which-key "marking")
 )
;; aa6a42b8 ends here

;; [[file:../../../gwp.note::03af13ba][03af13ba]]
;; since org 9
(org-link-set-parameters "zotero" :follow #'gwp/org-zotero-open :export #'gwp/org-zotero-export)

(defun gwp/org-zotero-open (path)
  (setq url (format "zotero:%s" path))
  (browse-url url))
;; 03af13ba ends here

;; [[file:../../../gwp.note::*ui][ui:1]]
;; rust-modules
(add-to-list 'load-path "/home/ybyygu/Workspace/Programming/emacs/rust-modules")
(require 'zotero)

(defun gwp/zotero-search-by-tag (name)
  "Search Zotero entries by tag using ivy."
  (interactive "sTag: ")

  (let* ((candidates (zotero-search-items-by-tag name)))
    (ivy-read (format "Zotero entries: ")
              candidates
              :action '(2               ; set the default action to open attachments
                        ("o" gwp--ivy-action-open-link "Open link")
                        ("O" gwp--ivy-action-open-attachments "Open attachments")
                        ("r" gwp--ivy-action-show-related-items "Show Related Items")
                        ("i" gwp--ivy-action-insert-link "Insert link")))))

(defun gwp/zotero-search-by-collection (name)
  "Search Zotero entries by collection name using ivy."
  (interactive "sCollection: ")

  (let* ((candidates (zotero-search-items-by-collection name)))
    (ivy-read (format "Zotero entries: ")
              candidates
              :action '(2               ; set the default action to open attachments
                        ("o" gwp--ivy-action-open-link "Open link")
                        ("O" gwp--ivy-action-open-attachments "Open attachments")
                        ("r" gwp--ivy-action-show-related-items "Show Related Items")
                        ("i" gwp--ivy-action-insert-link "Insert link")))))

(defun gwp--ivy-action-show-related-items (x)
  "show related items from selection"
  (let* ((candidates (zotero-get-related-items x)))
    (ivy-read (format "Related: ")
              candidates
              :action '(2               ; set the default action to open attachments
                        ("o" gwp--ivy-action-open-link "Open link")
                        ("O" gwp--ivy-action-open-attachments "Open attachments")
                        ("r" gwp--ivy-action-show-related-items "Show Related Items")
                        ("i" gwp--ivy-action-insert-link "Insert link")))))

(defun gwp--ivy-action-annotate-attachment (pdf-file)
  "Annotate the attachment with org-noter."
  (let ((annotation-file (expand-file-name (car org-noter-default-notes-file-names) (file-name-directory pdf-file))))
    (progn
      ;; create an empty annotation file if not exists
      (unless (file-exists-p annotation-file) (write-region "" nil annotation-file))
      (org-open-file pdf-file)
      (org-noter))))

(defun gwp--ivy-action-open-attachments (x)
  "ivy completion for zotero attachments."
  (let* ((candidates (zotero-get-selected-item-attachment-paths x)))
    (ivy-read (format "Open attachment: ")
              candidates
              :action '(1               ; set the default action to open link
                        ("o" org-open-file "Open")
                        ("n" gwp--ivy-action-annotate-attachment "Annotate")))))

(defun gwp--ivy-action-insert-link (x)
  (let ((uri (zotero-get-selected-item-link x)))
    (if uri
        (progn
          (message "%s!" x)
          (insert "[[" uri "][" "zotero-item" "]]"))
      (error "No link extracted from: %s" x))))

(defun gwp--ivy-action-open-link (x)
  (let ((uri (zotero-get-selected-item-link x)))
    (if uri
        (progn
          (message "%s!" x)
          (org-link-open-from-string (format "[[%s]]" uri)))
      (error "No link extracted from: %s" x))))

(defun gwp/org-open-zotero-attachments-at-point (arg)
  "Handle zotero attachments in org-mode"
  (interactive "P")
  (let ((ct (org-element-context)))
    (if (eq 'link (org-element-type ct))
        (let ((link (org-element-property :raw-link ct)))
          (when link
            (let ((key (zotero-get-item-key-from-link link)))
              (if key
                  (gwp--ivy-action-open-attachments key)
                (error "Invalid zotero link!"))))))))

(defun gwp/org-open-zotero-related-at-point (arg)
  "Open related zotero items for zotero link at point"
  (interactive "P")
  (let ((ct (org-element-context)))
    (if (eq 'link (org-element-type ct))
        (let ((link (org-element-property :raw-link ct)))
          (when link
            (let ((key (zotero-get-item-key-from-link link)))
              (if key
                  (gwp--ivy-action-show-related-items key)
                (error "Invalid zotero link!"))))))))

(defun gwp/insert-new-zotero-item (arg)
  "Create a new zotero item (report)"
  (interactive "P")

  (let ((uri (zotero-create-new-note)))
    (if uri
        (progn
          (message "%s!" uri)
          (insert "[[" uri "][" "zotero-note" "]]"))
      (error "create zotero item failed!"))))

;; https://www.reddit.com/r/emacs/comments/f3o0v8/anyone_have_good_examples_for_transient/
(require 'transient)
(transient-define-prefix gwp/zotero-search-transient ()
  "Search zotero database"
  [["Search zotero items:"
    ("t" "search by tag" gwp/zotero-search-by-tag)
    ("c" "search by collection" gwp/zotero-search-by-collection)
    ("o" "open attachments at point" gwp/org-open-zotero-attachments-at-point)
    ("r" "open related items at point" gwp/org-open-zotero-related-at-point)
    ]]
  )
;; ui:1 ends here

;; [[file:../../../gwp.note::a95fbcd5][a95fbcd5]]
(gwp::local-leader-def
 :keymaps 'org-mode-map
 "z" '(gwp/zotero-search-transient :which-key "zotero")
 )
;; a95fbcd5 ends here

;; [[file:../../../gwp.note::e121f679][e121f679]]
(require 'org-crypt)
(require 'epa-file)
(epa-file-enable)

;; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-crypt-tag-matcher "crypt")
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
(setq org-crypt-key "38D95BC6411A87E7") ; ybyygu@gmail.com
(setq org-crypt-disable-auto-save nil)
;; e121f679 ends here

;; [[file:../../../gwp.note::d3403c99][d3403c99]]
(require 'org-protocol)
;; d3403c99 ends here

;; [[file:../../../gwp.note::4ff602a9][4ff602a9]]
;;;###autoload
(defun gwp::org-enable-mouse ()
  (interactive)
  (require 'org-mouse)
  )

(map! :map gwp::org-toggle-map
      "m" #'gwp::org-enable-mouse
      )
;; 4ff602a9 ends here

;; [[file:../../../gwp.note::917381e9][917381e9]]
;; 2021-10-20: 可用 gwp::local-leader-def 来代替
;; 定义一些特别常用的命令, 仅在org-mode中显示
;; (defun gwp/org-mode-keys-hook ()
;;   ;; (evil-local-set-key 'normal (kbd "SPC RET") '+org/dwim-at-point)
;;   ;; (evil-local-set-key 'normal (kbd "SPC j t") 'gwp/org-babel-tangle-jump-to-file)
;;   ;; (evil-local-set-key 'normal (kbd "SPC d d") 'gwp/org-babel-tangle-dwim)
;; )
;; (add-hook 'org-mode-hook 'gwp/org-mode-keys-hook)
;; (defun gwp/org-src-mode-keys-hook ()
;;   (evil-local-set-key 'normal (kbd "SPC d d") 'gwp/org-babel-tangle-dwim))
;; (add-hook 'org-src-mode-hook 'gwp/org-mode-keys-hook)

;; [2021-10-04 Mon] leader map是全局性的, 不能动态加载和卸载
;; (map! :mode org-mode
;;       :leader
;;       ;; :when (equal major-mode 'org-mode)
;;       :desc "tangle blocks at point"      "o b" #'gwp/org-babel-tangle-dwim
;;       ;; :desc "execute in edit buffer"      "SPC" #'org-babel-do-key-sequence-in-edit-buffer
;;       ;; :desc "org-babel"                   "a"   org-babel-map;  换个容易按的键位
;;       :desc "Enter-dwim"                  "RET" #'+org/dwim-at-point
;;       )
;; 917381e9 ends here

;; [[file:../../../gwp.note::d3c71916][d3c71916]]
;; 经常按错这个键, 禁用之 (Ctrl-c ;)
(put 'org-toggle-comment 'disabled t)
(define-key! :keymaps 'org-mode-map "C-a" #'crux-move-beginning-of-line)

;; 保留以前的 Alt-Return 键行为, Alt-Return
(org-defkey org-mode-map [(meta return)] 'org-meta-return)
;; d3c71916 ends here

;; [[file:../../../gwp.note::bbdcd834][bbdcd834]]
(gwp::goto-leader-def
  :keymaps 'org-mode-map
  "k" '(org-up-element :which-key "goto up element")
  "j" '(org-next-visible-heading :which-key "next visible heading")
  ;; "h" '(org-beginning-of-line :which-key "goto the beginning of visible line")
  ;; "l" '(org-end-of-line :which-key "goto the end of visible line")
  ;; "k" '(org-backward-heading-same-level :which-key "backward heading")
  ;; "j" '(org-forward-heading-same-level :which-key "forward heading")
  )

(map! :map org-mode-map
      "M-l" #'org-metaright   ; doom中默认为 demote-subtree
      "M-h" #'org-metaleft    ; doom中默认为 promote-subtree
      "M-k" #'org-metaup
      "M-j" #'org-metadown
      "M-p" #'org-backward-element
      "M-n" #'org-forward-element
      )
;; bbdcd834 ends here
