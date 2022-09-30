;; [[file:../gwp.note::2ad3390b][2ad3390b]]
(require 'org-noter)
;; 2ad3390b ends here

;; [[file:../gwp.note::8a535ad4][8a535ad4]]
(defun gwp::org-note::create-annotation-file (document-path)
  (let* ((note-file "annotation.note")
         (document-name (file-name-nondirectory document-path))
         (document-base (file-name-base document-name)))
    (with-current-buffer (switch-to-buffer (find-file-noselect note-file))
      (goto-char (point-max))
      (insert "* " document-base)
      (org-set-property org-noter-property-doc-file document-name)
      (save-buffer))))

;;;###autoload
(defun gwp::org-note::dired-annotate-file-at-point ()
  "标注 dired buffer 中所定的(pdf)文件"
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (gwp::org-note::create-annotation-file (dired-get-file-for-visit))
    (user-error "not in dired buffer")))
;; 8a535ad4 ends here

;; [[file:../gwp.note::7f3b3bee][7f3b3bee]]
(defun gwp::org-note::to-read-file-in-READ (document-path read-dir)
  (let* ((document-name (file-name-nondirectory document-path))
         (document-in-read (concat read-dir document-name)))

    ;; 将当前文件复制至READ 目录下
    (message (format "%s => %s" document-path document-in-read))
    (rename-file document-path document-in-read)
    ;; 再将该文件反向软链回当前目录
    (make-symbolic-link (file-truename document-in-read) document-path)))

;;;###autoload
(defun gwp::org-note::dired-annotate-pdf-in-READ ()
  "将 dired buffer 中所选定的(pdf)文件放至READ 目录下"
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (let* ((read-dir (read-directory-name "分类目录: " "~/Boox/READ")))
        (gwp::org-note::to-read-file-in-READ (dired-get-file-for-visit) read-dir)
        (dired-do-redisplay))
    (user-error "not in dired buffer")))

;;;###autoload
(defun gwp::dired::symbol-link-move-back ()
  "在 dired 中, 将当前软链所指向的文件取回来, 同时删除源文件"
  (interactive)

  (if (derived-mode-p 'dired-mode)
      (let* ((this-file (dired-get-file-for-visit))
             (target-path (file-truename this-file)))
        (if (file-symlink-p this-file)
            (when (file-exists-p target-path)
              (delete-file this-file)
              (rename-file target-path this-file 1)
              (dired-do-redisplay)
              (message "Moved from: %s" target-path))
          (user-error "not a symlink file")))
    (user-error "not in dired buffer")))
;; 7f3b3bee ends here

;; [[file:../gwp.note::1773f1a3][1773f1a3]]
(defun gwp::org-note::get-pdf-file ()
  (save-excursion
    (if (search-backward ":NOTER_DOCUMENT" nil t)
        (progn
          (org-back-to-heading)
          (let ((pdf (org-element-property :NOTER_DOCUMENT (org-element-at-point))))
            (message "%s" pdf)))
      (message "no pdf doc found"))))

(defun gwp::org-note::get-pdf-page ()
  (save-excursion
    (org-back-to-heading)
    (let ((property (org-element-property :NOTER_PAGE (org-element-at-point))))
      (let ((value (car (read-from-string property))))
        (cond
         ((consp value) (car value))
         (t value))))))

;;;###autoload
(defun gwp::org-note::new-note ()
  "在当前 heading 下插入新的文献阅读笔记"
  (interactive)
  ;; (let ((current-prefix-arg '(4)))     ; C-u
  ;;   (call-interactively #'org-insert-heading))
  ;; (insert (read-string "笔记标题: "))
  (if (org-at-heading-p)
      (let ((page (read-number "PDF 页码: " 1)))
        (org-set-property "NOTER_PAGE" (number-to-string page)))
    (user-error "not at org heading.")))

;;;###autoload
(defun gwp::org-note::open-pdf ()
  "使用 llpp 来打开当前笔记对应的 pdf 文件, 并转到指定的页码"
  (interactive)
  (let ((page (gwp::org-note::get-pdf-page))
        (pdf (gwp::org-note::get-pdf-file)))
    (if page
        (start-process "llpp" nil "llpp" pdf "-page" (format "%s" page))
      ;; (start-process "okular" nil "okular" pdf "-p" (format "%s" page))
      (start-process "llpp" nil "llpp" pdf)
      ;; (start-process "okular" nil "okular" pdf)
      )))
;; 1773f1a3 ends here

;; [[file:../gwp.note::8d4b377b][8d4b377b]]
(provide 'init-note)
;; 8d4b377b ends here
