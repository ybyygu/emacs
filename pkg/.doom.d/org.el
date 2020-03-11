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

;; [[file:~/Workspace/Programming/emacs/doom.note::*dwim-open-at-point][dwim-open-at-point:1]]
;; https://stackoverflow.com/questions/17590784/how-to-let-org-mode-open-a-link-like-file-file-org-in-current-window-inste
;; Depending on universal argument try opening link
(defun gwp/org-open-at-point-dwim (&optional arg)
  (interactive "P")
  (if arg (let ((org-link-frame-setup (quote ((file . find-file)))))
            (org-open-at-point)
            )
    (let ((org-link-frame-setup (quote ((file . find-file-other-window)))))
      (org-open-at-point)
      (zoom))
    ))
(map! :map org-mode-map "C-c C-o" #'gwp/org-open-at-point-dwim)
;; dwim-open-at-point:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*jump][jump:1]]
;; https://emacs.stackexchange.com/questions/50649/jumping-from-a-source-block-to-the-tangled-file
(defun gwp/org-babel-tangle-jump-to-file ()
  "Jump to tangle file for the source block at point."
  (interactive)
  (let (file org-babel-pre-tangle-hook org-babel-post-tangle-hook)
    (cl-letf (((symbol-function 'write-region) (lambda (start end filename &rest _ignore)
                         (setq file filename)))
          ((symbol-function 'delete-file) #'ignore))
      (org-babel-tangle '(4)))
    (when file
      (setq file (expand-file-name file))
      (if (file-readable-p file)
      (find-file file)
    (error "Cannot open tangle file %S" file)))))
;; jump:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*tangle][tangle:1]]
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

;; [[file:~/Workspace/Programming/emacs/doom.note::*tangle][tangle:2]]
(defun gwp/org-edit-save-and-tangle ()
  "when in a sub-editing buffer, swith to the parent buffer and tangle the file blocks"
  (interactive)
  (when (buffer-modified-p) (org-edit-src-save))
  (org-edit-src-exit)
  (call-interactively 'gwp/org-babel-tangle-blocks)
  (org-edit-src-code)
  )

(defun gwp/org-babel-tangle-dwim()
  "tangle current file blocks whenever in a sub-editing buffer or not"
  (interactive)
  (save-excursion
    (if (org-src-edit-buffer-p) (call-interactively 'gwp/org-edit-save-and-tangle)
      (call-interactively 'gwp/org-babel-tangle-blocks)
      )
    )
  )
;; tangle:2 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*bindings][bindings:1]]
(map! :map org-mode-map
      :localleader
      (:prefix ("b" . "org-babel")
        :desc "check src block headers"    "c" #'org-babel-check-src-block
        :desc "insert header argument"     "i" #'org-babel-insert-header-arg
        :desc "view header arguments"      "I" #'org-babel-view-src-block-info
        :desc "demarcate block"            "d" #'org-babel-demarcate-block
        :desc "edit src codes in place"    "s" #'gwp/org-babel-edit-structure-in-place
        :desc "jump to file tangled file"  "j" #'gwp/org-babel-tangle-jump-to-file
        :desc "execute in edit buffer"     "x" #'org-babel-do-key-sequence-in-edit-buffer
        :desc "tangle blocks at point"     "b" #'gwp/org-babel-tangle-dwim
        :desc "tangle blocks in subtree"   "t" #'gwp/org-tangle-subtree
        :desc "tangle blocks in buffer"    "T" #'org-babel-tangle
        )
      ;; 为了顺应spacemacs中的设置, 保留spc-ob 按键
      :leader
      :desc "tangle blocks at point" "o b" #'gwp/org-babel-tangle-dwim
      )
;; bindings:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*org-noter][org-noter:1]]
(use-package! org-noter
  :after org-mode
  )
;; org-noter:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*dwim-enter-at-point][dwim-enter-at-point:1]]
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
;; dwim-enter-at-point:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*screenshot][screenshot:1]]
(defun gwp/org-image-attributes-default (&optional caption)
  "default image attributes: caption, name label, width ..."
    "Annotate LINK with the time of download."
    (format (concat
             (format "#+DOWNLOADED: %s @ %s\n"
                     (if (equal link org-download-screenshot-file)
                         "screenshot"
                       link)
                     (format-time-string "%Y-%m-%d %H:%M:%S"))
             (concat  "#+caption: " (read-string "Caption: " caption) "\n")
             ;; set unique figure name
             (format "#+name: fig:%s\n" (substring (org-id-new) 0 8))
             ;; unit in px; for displaying in org-mode
             "#+attr_org: :width 800\n"
             ;; unit in cm; for exporting as odt
             "#+attr_odt: :width 10\n"
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
                      org-download-annotate-function 'gwp/org-download-annotate
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

;; [[file:~/Workspace/Programming/emacs/doom.note::*template][template:1]]
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
    )
 )
;; template:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*auto time-stamp][auto time-stamp:1]]
(with-eval-after-load "ob-tangle"
  ;; update timestamps on tangled files
  (setq time-stamp-pattern "100/UPDATED:[ \t]+\\\\?[\"<]+%:y-%02m-%02d %3a %02H:%02M\\\\?[\">]")
  (defun org-babel-post-tangle-hook--time-stamp ()
    "Update timestamps on tangled files."
    (time-stamp)
    (save-buffer))
  (add-hook 'org-babel-post-tangle-hook 'org-babel-post-tangle-hook--time-stamp))
;; auto time-stamp:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*zotero][zotero:1]]
(with-eval-after-load 'org-compat

  ;; since org 9
  (org-link-set-parameters "zotero" :follow #'gwp/org-zotero-open :export #'gwp/org-zotero-export)

  (defun gwp/org-zotero-open (path)
    (setq url (format "zotero:%s" path))
    (browse-url url)
    )

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
;; zotero:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*odt export][odt export:1]]
(use-package ox-odt
  :config
  (progn
    ;; continually numbering captions without outline level
    (setq org-odt-display-outline-level 0)

    ;; useful for odt export using dvipng
    (setq org-format-latex-options (plist-put org-format-latex-options :html-scale 3.0))
    (setq org-odt-pixels-per-inch 300.0)
    )
  )
;; odt export:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*odt export][odt export:2]]
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

;; [[file:~/Workspace/Programming/emacs/doom.note::*capture & protocol][capture & protocol:1]]
(setq org-capture-templates
      '(
        ;; ("i" "interleave" plain (file "~/Incoming/annotation.note")
        ;;  "#+setupfile: ~/Notes/common.org\n#+ZOTERO_ITEM: %x\n#+INTERLEAVE_PDF: %?\n" :prepend t :kill-buffer t)
        ("n" "Note" entry (file "~/Notes/refile.note")
         "* %u %? [[%:link][%:description]]\n  %:initial\n" :prepend t)
        ("t" "Task" entry (file+headline "~/Notes/life.note" "Tasks")
         "* TODO %^T\n  %i" :prepend t)
        ("r" "Research Memo" entry (file+headline "~/Notes/research.note" "Memo")
         "* %u %?\n  %i\n" :prepend t)
        ("p" "Paper" entry (file+headline "~/Notes/research.note" "References")
         "* %u %? %x\n  %i\n" :prepend t)
        ("j" "Life Journal" entry (file+headline "~/Notes/life.note" "Journals")
         "* %u %?\n  %i\n" :prepend t)
        )
      )
;; capture & protocol:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*encryption][encryption:1]]
(require 'org-crypt)
(require 'epa-file)

(epa-file-enable)

;; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-crypt-tag-matcher "crypt")
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
                                        ; GPG key to use for encryption
(setq org-crypt-key "38D95BC6411A87E7") ; ybyygu@gmail.com
(setq org-crypt-disable-auto-save nil)
;; encryption:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*setup][setup:1]]
(require 'org-attach)
;; org-mode添加附件后会自动commit(git only), 禁掉:
;; (setq org-attach-commit nil)
;; setup:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*copy & paste attachments][copy & paste attachments:1]]
(setq org-attach-store-link-p 'attached)
;; copy & paste attachments:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*copy & paste attachments][copy & paste attachments:2]]
;; 1. store the directory
(defun gwp/org-attach-store (&optional force)
  "store org attachment directory of current enetry"
  (interactive "P")
  ;; make a temporary symlink to store the attachment path
  (setq file-attach-tmp (concat spacemacs-cache-directory ".gwp-attach-tmp"))
  (let ((attach-dir (org-attach-dir)))
    (when attach-dir
      (progn
        ;; remove existing directory
        (when (file-directory-p file-attach-tmp) (delete-directory file-attach-tmp t))
        ;; remove existing file and symlink
        (when (file-exists-p file-attach-tmp) (delete-file file-attach-tmp))
        ;; remove broken symlink
        (when (file-symlink-p file-attach-tmp) (delete-file file-attach-tmp))
        (make-symbolic-link attach-dir file-attach-tmp)
        (message (format "stored to: %s" file-attach-tmp))
        )
      )
    )
  )
;; copy & paste attachments:2 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*copy & paste attachments][copy & paste attachments:3]]
;; 2. move the stored directory to new location
(defun gwp/org-attach-move (&optional force)
  "move stored attachments to current entry"
  (interactive "P")
  ;; ~/.emacs.d/.cache/.gwp-attach-tmp
  (setq file-attach-tmp (concat spacemacs-cache-directory ".gwp-attach-tmp"))

  (if (file-exists-p file-attach-tmp)
      ;; create attachment directory if not exists using org-attach-dir function
      (let ((attach-dir (org-attach-dir t)))
        (progn
          ;; read old attach directory from previous stored symlink
          (setq attach-dir-old (file-chase-links file-attach-tmp))
          ;; sanity check
          (if (y-or-n-p (format "%s/* ==> %s ?" attach-dir-old attach-dir))
              (progn
                (shell-command (format "mv %s/* %s" attach-dir-old attach-dir))
                ;; remove stale tmp-link
                (delete-file file-attach-tmp)
                )
            (message "cancelled")
            )
          )
        )
    (message (format "no stored symbolic link found: %s" file-attach-tmp))
    )
  )
;; copy & paste attachments:3 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*从当前位置文件链接提取文件名.][从当前位置文件链接提取文件名.:1]]
(defun gwp/org-file-link-p (&optional element)
  (let ((el (or element (org-element-context))))
    (and (eq (org-element-type el) 'link)
         (string= (org-element-property :type el) "file")
         )
    )
  )
;; 从当前位置文件链接提取文件名.:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*从当前位置文件链接提取文件名.][从当前位置文件链接提取文件名.:2]]
(defun gwp/file-path-at-point()
  "get file path from link at point"
  (let ((el (org-element-context)))
    (when (gwp/org-file-link-p el)
      (org-element-property :path el)
      )
    )
  )
;; 从当前位置文件链接提取文件名.:2 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*使用org-attach将文件move到当到附录中并更新文件链接][使用org-attach将文件move到当到附录中并更新文件链接:1]]
;; (require 'org-download)

(defun gwp/org-store-link-without-desc (file)
  "store file link without the description part -- a tweak to make odt image exporting correct."
  (setq org-stored-links
        (cons (list (org-attach-expand-link (file-name-nondirectory file)) "")
              org-stored-links)
        )
  )

(defun gwp/org-take-as-local-attachment ()
  "move file link at point as local attachment"
  (interactive)
  (let ((file (gwp/file-path-at-point)))
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
          (org-display-inline-images)
         )
      (user-error "Point is not on a link")
      )
    )
  )
;; 使用org-attach将文件move到当到附录中并更新文件链接:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*TODO refile][refile:1]]
;; any headline with level <= 2 is a target
(setq org-refile-targets '(
                           (org-agenda-files :tag . "Incoming")
                           )
      )

(setq org-reverse-note-order t)
(defun gwp/get-org-file-link-path ()
  (save-excursion
    (beginning-of-line)
    (search-forward "[[file:" (line-end-position))
    (if (org-in-regexp org-bracket-link-regexp 1)
        (org-link-unescape (match-string-no-properties 1))
      )
    )
  )

(defun gwp/enter-to-read-state()
  "evoke external shell script when entering READ state"
  (when (equal org-state "READ")
    (setq file (gwp/get-org-file-link-path))
    (if file
        (progn
         (setq cmd (concat "org-to-read.sh " (shell-quote-argument file)))
         (message cmd)
         (shell-command cmd)
        )
        )
    )
    (when (equal org-last-state "READ")
      (message "try to remove READ state")
      (setq file (gwp/get-org-file-link-path))
      (if file
          (progn
            (setq cmd (concat "org-read-done.sh " (shell-quote-argument file)))
            (message cmd)
            (shell-command cmd)
            )
        )
      )
  )
(add-hook 'org-after-todo-state-change-hook 'gwp/enter-to-read-state)

;; show a sparse-tree in READ keyword
(defun gwp/org-show-read-tree ()
  "show a sparse-tree in READ keyword"
  (interactive)

  (let ((base-vector [?\C-u ?\M-x ?o ?r ?g ?- ?s ?h ?o ?w ?- ?t ?o ?d ?o ?- ?t ?r ?e ?e return ?R ?E ?A ?D return]))
    ;; create new macro of the form
    ;; C-u M-x org-show-todo-tree RET READ RET
    (execute-kbd-macro (vconcat base-vector
                                (vector 'return)))))
;; refile:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*agenda][agenda:1]]
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
  (setq org-agenda-include-all-todo nil)
  )
;; agenda:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*agenda][agenda:2]]
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
;; agenda:2 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*agenda][agenda:3]]
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

;; [[file:~/Workspace/Programming/emacs/doom.note::*misc][misc:1]]
;; (require 'org-man)
;; misc:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*misc][misc:2]]
(setq org-fontify-emphasized-text nil)
;; misc:2 ends here
