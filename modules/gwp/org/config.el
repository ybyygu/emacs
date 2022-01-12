;; [[file:../../../gwp.note::b28b06dc][b28b06dc]]
(require 'org)
(setq org-blank-before-new-entry nil)
(setq org-default-notes-file (concat org-directory "/life.note"))

;; 禁用字词检查, 需要了再开
(remove-hook! 'org-mode-hook #'flyspell-mode)
(remove-hook! 'text-mode-hook #'spell-fu-mode)
;; b28b06dc ends here

;; [[file:../../../gwp.note::7341aa84][7341aa84]]
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

;; 显示光标所在处的headline
(defun gwp::org-show-context-at-point ()
  (interactive)
  ;; 从下面的命令看来的
  ;; (call-interactively 'org-mark-ring-goto)
  ;; (org-show-context 'mark-goto)
  (org-show-subtree)
  (when (or (org-invisible-p) (org-invisible-p2)) (org-show-context 'mark-goto))
  (call-interactively 'org-reveal))

;; (map! :map org-mode-map
;;       :n "zo" #'gwp::org-show-context-at-point
;;       :n "zc" #'org-hide-entry)
;; 7341aa84 ends here

;; [[file:../../../gwp.note::d3c71916][d3c71916]]
;; 经常按错这个键, 禁用之 (Ctrl-c ;)
(put 'org-toggle-comment 'disabled t)
(define-key! :keymaps 'org-mode-map "C-a" #'crux-move-beginning-of-line)

;; 保留以前的 Alt-Return 键行为, Alt-Return
(org-defkey org-mode-map [(meta return)] 'org-meta-return)
;; d3c71916 ends here

;; [[file:../../../gwp.note::*narrow][narrow:1]]
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
;; narrow:1 ends here

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
