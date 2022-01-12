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
