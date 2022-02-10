;; [[file:../../../gwp.note::e4fc036b][e4fc036b]]
;; 要保证 C-u C-@ 连续调用有效
(setq set-mark-command-repeat-pop nil)

(defun gwp::jump-to-previous-mark ()
  (interactive)
  (let ((current-prefix-arg '(4)))     ; C-u
    (call-interactively #'set-mark-command)))

;; 根据手册: C-SPC C-SPC两次
;; 仅标记当前位置(push to mark ring), 但不选择
(defun gwp::mark-current-position ()
  (interactive)
  (call-interactively #'set-mark-command)
  (deactivate-mark)
  )

(defun gwp::mark-and-save-buffer()
  "标记光标所在位置, 并保存buffer"
  (interactive)
  (call-interactively #'gwp::mark-current-position)
  (save-buffer))

(setq global-mark-ring-max 99
      mark-ring-max 99)
;; e4fc036b ends here

;; [[file:../../../gwp.note::e48dc36a][e48dc36a]]
;; https://stackoverflow.com/a/27661338
(defun marker-is-point-p (marker)
  "test if marker is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

(defun push-mark-maybe ()
  "push mark onto `global-mark-ring' if mark head or tail is not current location"
  (if (not global-mark-ring) (error "global-mark-ring empty")
    (unless (or (marker-is-point-p (car global-mark-ring))
                (marker-is-point-p (car (reverse global-mark-ring))))
      (push-mark))))


(defun backward-global-mark ()
  "use `pop-global-mark', pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark))

(defun forward-global-mark ()
  "hack `pop-global-mark' to go in reverse, pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (setq global-mark-ring (nreverse global-mark-ring))
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark)
  (setq global-mark-ring (nreverse global-mark-ring)))
;; e48dc36a ends here

;; [[file:../../../gwp.note::ebb32bb1][ebb32bb1]]
;; https://github.com/deestan/emacs/blob/master/emacs-goodies-el/marker-visit.el
;;
;;; marker-visit.el --- navigate through a buffer's marks in order

;; Copyright (C) 2001 Benjamin Rutt
;;
;; Maintainer: Benjamin Rutt <brutt@bloomington.in.us>
;; Version: 1.1

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, send e-mail to
;; this program's maintainer or write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file provides a simple way to navigate among marks in a
;; buffer.  C-u C-SPC is similar, but takes you haphazardly around the
;; buffer.  Setting bookmarks is a lot of extra work if you just want
;; to jump around your buffer quickly; plus, you have to come up with
;; a name for every bookmark.

;; All the marks you've left while editing a buffer serve as bread
;; crumb trails of areas in the buffer you've edited.  It is
;; convenient to navigate back and forth among these marks in order.
;; This file provides two methods to do just that, marker-visit-prev
;; and marker-visit-next.  These two functions will take you, from
;; point, to the nearest mark in either direction.  The function
;; marker-visit-truncate-mark-ring will truncate the mark ring.

;; The marks you can visit in a buffer consist of: "the mark" plus the
;; contents of the mark-ring.

;;; Usage:

;; put this file in your load-path and add the line
;;
;; (require 'marker-visit)
;;
;; to your ~/.emacs file.
;;
;; This package is most useful when some easy-to-press keys are bound
;; to the functions marker-visit-prev and marker-visit-next.  See C-h
;; i m Emacs RET m Key Bindings RET for info on emacs key bindings.

;;; History:

;; 1.0 -> 1.1 Incorporated patch from Colin Walters to make the code
;; consistent with elisp code conventions mentioned in
;; (Info-goto-node "(elisp) Coding Conventions").

;;; Code:

;;utility remove-dupes function
(defun marker-visit-remove-dupes (ls)
  (cond
   ((null ls) '())
   ((member (car ls) (cdr ls)) (marker-visit-remove-dupes (cdr ls)))
   (t (cons (car ls) (marker-visit-remove-dupes (cdr ls))))))

;;create a sorted list of marks, including the point as mark, the
;;mark, and the contents of the mark-ring.
(defun marker-visit-get-sorted-mark-set (current-point-mark)
  (marker-visit-remove-dupes
   (sort
    (append (cons current-point-mark
		  (if (mark-marker) (list (mark-marker)) nil))
	    (mapcar (lambda (id) id) mark-ring))
    (lambda (a b) (< a b)))))

(defun marker-visit-no-markers-p ()
  (and (null mark-ring)
       (or (not (mark-marker))
	   (not (marker-position (mark-marker))))))

(defun marker-visit-warn (error-message)
  (message error-message)
  (beep))

;;;###autoload
(defun marker-visit-prev ()
  "From point, visit the nearest mark earlier in the buffer."
  (interactive)
  (if (marker-visit-no-markers-p)
      (marker-visit-warn "Mark does not point anywhere")
    (let* ((current-point-mark (point-marker))
	   (sorted-marks (marker-visit-get-sorted-mark-set current-point-mark))
	   (dest-mark nil))
      (while (not (equal current-point-mark (car sorted-marks)))
	(setq dest-mark (car sorted-marks))
	(setq sorted-marks (cdr sorted-marks)))
      (if dest-mark
	  (goto-char dest-mark)
	(marker-visit-warn "No previous mark to visit")))))

;;;###autoload
(defun marker-visit-next ()
  "From point, visit the nearest mark later in the buffer."
  (interactive)
  (if (marker-visit-no-markers-p)
      (marker-visit-warn "Mark does not point anywhere")
    (let* ((current-point-mark (point-marker))
	   (sorted-marks (marker-visit-get-sorted-mark-set current-point-mark))
	   (dest-mark nil)
	   (done nil))
      (while (not done)
	(if (equal current-point-mark (car sorted-marks))
	    (progn
	      (setq dest-mark (cadr sorted-marks))
	      (setq done t))
	  (setq sorted-marks (cdr sorted-marks))))
      (if dest-mark
	  (goto-char dest-mark)
	(marker-visit-warn "No next mark to visit")))))

;;;###autoload
(defun marker-visit-truncate-mark-ring ()
  "Truncate the `mark-ring'."
  (interactive)
  (setq mark-ring nil))
;; ebb32bb1 ends here

;; [[file:../../../gwp.note::00b43976][00b43976]]
(defhydra gwp::hydra-mark-ring-pop ()
  "goto last location"
  ("SPC" gwp::jump-to-previous-mark "prev mark")          ; 在org中可自动打开折叠的内容
  ("j" marker-visit-next "next mark")
  ("k" marker-visit-prev "prev mark")                     ; NOTE: org折叠的内容不会打开
  ("C-p" backward-global-mark "prev mark (global)")         ;
  ("C-n" forward-global-mark "next mark (global)")          ;
  ("r" gwp::org-show-context-at-point "org show context") ; 在org时: 跳转到被折叠的headline中很有用
  ("q" nil "quit"))
;; 00b43976 ends here

;; [[file:../../../gwp.note::d1047b4d][d1047b4d]]
(use-package crux
  :config
  (define-key! [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  ;; 这个不管用, define-key! 管用
  ;; (map! :ni "C-a" 'crux-move-beginning-of-line)
  ;; (map! [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  )
;; d1047b4d ends here

;; [[file:../../../gwp.note::*auto-save][auto-save:1]]
(setq
 ;; doom里已默认为true
 auto-save-default t
 ;; 默认为5秒. 这里改大一些, 避免编辑时自动保存太快, 光标前的空格被吞掉
 auto-save-visited-interval 30)

;; 自动保存至当前文件名, 而临时文件
(auto-save-visited-mode +1)
;; auto-save:1 ends here

;; [[file:../../../gwp.note::e571c476][e571c476]]
(use-package simpleclip)

;; 从其它程序复制的内容也放至在kill-ring中, 不会因为emacs的操作而覆盖之前的内容
(setq save-interprogram-paste-before-kill t)
;; e571c476 ends here

;; [[file:../../../gwp.note::b5a74212][b5a74212]]
(setq kill-ring-max 999)

;; 粘贴时删除区域中的内容, 不污染clipboard, 方便连续yank.
(defun gwp::yank-dwim (arg)
  "粘贴并覆盖选定区域. 如果以C-u调用则提示从kill-ring中选择"
  (interactive "P")
  (when (region-active-p)
    (call-interactively #'delete-region))
  (if (equal arg '(4))                  ; C-u
      (call-interactively #'counsel-yank-pop)
    (call-interactively #'yank)))
(map! "C-y" #'gwp::yank-dwim)
(gwp::text-edit-def "C-v" #'gwp::yank-dwim)

;; 保持和terminal中的行为一致: 删除选定区域或向后一个单词
(defun gwp::ctrl-w-dwim ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-region)
    (call-interactively #'backward-kill-word)))

(map! "C-w" #'gwp::ctrl-w-dwim); cut, copy: Alt-w
;; 删除到行尾或删除整行
(map! "C-k"  #'crux-smart-kill-line)
;; b5a74212 ends here

;; [[file:../../../gwp.note::7d5caf69][7d5caf69]]
(defun gwp::ctrl-d-dwim (prefix)
  "清除区域或复制区域"
  (interactive "P")
  (if prefix                  ; C-u
      (call-interactively #'gwp::duplicate-line-or-region)
    (call-interactively #'gwp::delete-char-or-region)))

(defun gwp::delete-char-or-region()
  "清除光标前字符或选定区域"
  (interactive)
  (if mark-active
      (call-interactively #'delete-region)
    (delete-char 1)))

(map! "C-d" #'gwp::ctrl-d-dwim)
;; 7d5caf69 ends here

;; [[file:../../../gwp.note::*keyfreq][keyfreq:1]]
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
;; keyfreq:1 ends here

;; [[file:../../../gwp.note::ab440ea2][ab440ea2]]
(defun gwp::insert-date (arg)
  "Insert date at point. With prefix argument, insert date and time."
  (interactive "P")
  (insert (format-time-string "%Y-%m-%d"))
  (when arg
    (insert (format-time-string " %H:%M"))))

;; make it easier to update time-stamp
(gwp::text-edit-def "C-c i" #'gwp::insert-date)
;; ab440ea2 ends here

;; [[file:../../../gwp.note::7628d03d][7628d03d]]
;; https://endlessparentheses.com/disable-mouse-only-inside-emacs.html
(define-minor-mode disable-mouse-mode
  "A minor-mode that disables all mouse keybinds."
  :global t
  :lighter " 🐭"
  :keymap (make-sparse-keymap)

  (dolist (type '(mouse
                  down-mouse
                  drag-mouse
                  double-mouse
                  triple-mouse))
    (dolist (prefix '("" C- M- S- M-S- C-M- C-S- C-M-S-))
      ;; Yes, I actually HAD to go up to 7 here.
      (dotimes (n 3)
        (let ((k (format "%s%s-%s" prefix type n)))
          (define-key disable-mouse-mode-map
            (vector (intern k)) #'ignore))))))

(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "禁用鼠标" "m" #'disable-mouse-mode
       ))

(defun turn-off-disable-mouse-mode ()
  (disable-mouse-mode -1))

(defun turn-on-disable-mouse-mode ()
  (disable-mouse-mode 1))

;; 在insert状态下禁用鼠标, 避免误碰触控板
(add-hook! 'evil-insert-state-entry-hook #'turn-on-disable-mouse-mode)
(add-hook! 'evil-insert-state-exit-hook #'turn-off-disable-mouse-mode)
;; 7628d03d ends here

;; [[file:../../../gwp.note::1c79ba79][1c79ba79]]
(defhydra gwp::hydra-last-change ()
  ("j" goto-last-change "last change")  ; 用 p 按键容易误操作, 用N 安全些
  ("k" goto-last-change-reverse "previous change")
  ("c" recenter "recenter")
  ("r" gwp::org-show-context-at-point "org show context")
  ("q" nil "quit"))
;; 1c79ba79 ends here

;; [[file:../../../gwp.note::7db2aa5a][7db2aa5a]]
;; Get rid of `yas-expand' binding on TAB. Cannot do this from the `:bind'
;; section, annoyingly. And other annoyinglies as well. See:
;;   (spydez/help/issue/visit "yasnippet" "unbind-tab.org")
;; for more details.
(use-package yasnippet
  :config
  (unbind-key "TAB" yas-minor-mode-map)
  (unbind-key "<tab>" yas-minor-mode-map)
  ;; 不用TAB, 因为要避免 org 中与 tab 键冲突
  (define-key yas-minor-mode-map (kbd "<C-i>") 'yas-expand)
  )
;; 7db2aa5a ends here

;; [[file:../../../gwp.note::9786fedc][9786fedc]]
(defhydra gwp/hydra-smartparens (:hint nil)
  ("v" evil-visual-char)
  ("u" evil-undo)
  ("h" evil-backward-char)
  ("l" evil-forward-char)
  ("j" evil-next-line)
  ("k" evil-previous-line)
  ("(" sp-wrap-round "wrap in (round)")
  ("[" sp-wrap-square)
  ("{" sp-wrap-curly)
  ("'"  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "'")))
  ("\""  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\"")))
  ("d" sp-unwrap-sexp "unwrap pair")
  ("q" nil "quit")
  )
;; 9786fedc ends here

;; [[file:../../../gwp.note::ba461562][ba461562]]
(defun gwp::mark-whole-line ()
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))
;; ba461562 ends here

;; [[file:../../../gwp.note::be09bc09][be09bc09]]
;; expand selection
;; http://xahlee.org/emacs/modernization_mark-word.html
;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))

(defun gwp/select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters are paired characters: ()[]<>«»“”‘’「」, including \"\"."
  (interactive)
  (let (b1)
    (skip-chars-backward "^<>(“{[「«\"‘")
    (setq b1 (point))
    (skip-chars-forward "^<>)”}]」»\"’")
    (point)
    (set-mark (- b1 1))))

(defun gwp/select-none-blank-text ()
  "选择光标下非空格文字"
  (interactive)
  (let (b1)
    (skip-chars-backward "^ \n")
    (setq b1 (point))
    (skip-chars-forward "^ \n")
    (backward-char 1)
    (point)
    (set-mark b1)))

(defun gwp/select-word-dwim ()
  "选择连续的英文字词(不包括汉字)"
  (interactive)
  (let ((regexp "[\.-_A-Za-z0-9]") b1)
    (when (or (looking-at regexp)
              (er/looking-back-on-line regexp))
      (skip-chars-backward regexp)
      (setq b1 (point))
      (skip-chars-forward regexp)
      (backward-char)
      (point)
      (set-mark b1))))

;; https://github.com/magnars/expand-region.el
;; (require 'expand-region)
;; (global-set-key (kbd "M-4") 'er/expand-region)
;; be09bc09 ends here

;; [[file:../../../gwp.note::8fac8bf1][8fac8bf1]]
;; (use-package expand-region :after evil :config
;;   (map! :leader :v "v"
;;         (function er/expand-region)))

(require 'transient)
(transient-define-prefix gwp/advanced-selection ()
  "Advanced selection"
  [["常规选择"
    ("p" "select paragraph" er/mark-paragraph)
    ("c" "select comment" er/mark-comment)
    ("b" "select none blank" gwp/select-none-blank-text)
    ("t" "select text in quote" gwp/select-text-in-quote)
    ("w" "select word" gwp/select-word-dwim)
    ]]
  [["特殊选择"
    ("u" "mark url" er/mark-url)
    ("c" "mark org code block" er/mark-org-code-block)
    ("e" "mark email" er/mark-email)
    ]]
  )
;; 8fac8bf1 ends here

;; [[file:../../../gwp.note::b9054953][b9054953]]
(map! "M-u" #'upcase-dwim
      "M-l" #'downcase-dwim
      "M-c" #'capitalize-dwim)
;; b9054953 ends here

;; [[file:../../../gwp.note::3eff5fa2][3eff5fa2]]
(defun gwp::duplicate-region (beg end)
  (interactive "r")
  (save-excursion
    (let* ((beg (or beg (region-beginning)))
           (end (or end (region-end)))
           (region (buffer-substring beg end)))
      (goto-char end)
      (insert region))))

(defun gwp::duplicate-line (&optional stay)
  (save-excursion
    (move-end-of-line nil)
    (save-excursion
      (insert (buffer-substring (point-at-bol) (point-at-eol))))
    (newline)))

(defun gwp::duplicate-line-or-region()
  "复制当前行或选定区域"
  (interactive)
  (if (region-active-p)
      ;; 也可用 crux-duplicate-current-line-or-region
      (call-interactively #'gwp::duplicate-region)
    (gwp::duplicate-line)))

(map! :leader "C-d" #'delete-duplicate-lines)

;; (use-package move-dup
;;   :config
;;   (map! :iv "M-j" #'move-dup-move-lines-down)
;;   (map! :iv "M-k" #'move-dup-move-lines-up)
;;   (map! :iv "C-M-j" #'move-dup-duplicate-down)
;;   (map! :iv "C-M-k" #'move-dup-duplicate-up))
;; 3eff5fa2 ends here

;; [[file:../../../gwp.note::6ded2bf1][6ded2bf1]]
(map!
      [M-mouse-4] #'better-jumper-jump-backward
      [M-mouse-5] #'better-jumper-jump-forward)

(map! :leader
      (:prefix-map ("j" . "jump")
       (:prefix-map ("a" . "avy")
        :desc "Search and jump (pinyin)"        "c" #'avy-goto-char-2
        :desc "Search and jump"                 "s" #'avy-goto-char-timer
        :desc "jump to line"                    "l" #'avy-goto-line
        )))
;; 6ded2bf1 ends here

;; [[file:../../../gwp.note::08a09ddb][08a09ddb]]
(map! :map isearch-mode-map
      :desc "avy跳转" "C-c ;" #'avy-isearch
      )
;; 08a09ddb ends here

;; [[file:../../../gwp.note::0ed10c98][0ed10c98]]
(defun gwp/evil-ex-search-avy-jump ()
  (interactive)
  (when evil-ex-search-pattern
    (let ((regex (car evil-ex-search-pattern)))
      (avy-jump regex)
      (evil-ex-search-stop-session))))

;; (map! :map evil-ex-search-keymap
;;       :desc "avy jump" "C-c ;" #'gwp/evil-ex-search-avy-jump)
;;
;; 0ed10c98 ends here

;; [[file:../../../gwp.note::dde3ee55][dde3ee55]]
(map! :map ivy-minibuffer-map
      ;; 修改后的命令会报错, 原按键可以. 原因不明.
      ;; "C-c ;" #'ivy-avy
      :desc "二次过滤" "C-c r" (general-simulate-key "S-SPC")
      :desc "avy跳转" "C-c ;" (general-simulate-key "C-'")
      :desc "上一页" "C-c C-p" #'ivy-scroll-down-command
      :desc "下一页" "C-c C-n" #'ivy-scroll-up-command
      )
;; dde3ee55 ends here

;; [[file:../../../gwp.note::1a0721e0][1a0721e0]]
(defun gwp::smart-open-line-above ()
  (interactive)
  (if (featurep 'evil)
      (when (evil-normal-state-p)
        (evil-insert 1)))
  (call-interactively #'crux-smart-open-line-above))

(defun gwp::smart-open-line ()
  (interactive)
  (if (featurep 'evil)
      (call-interactively 'evil-open-below)
    (end-of-line)
    (newline-and-indent)))

;; M-o 默认为set-face之类的东西
(gwp::text-edit-def
 "M-o" #'gwp::smart-open-line-above
 "C-j" #'gwp::smart-open-line
 )
;; 1a0721e0 ends here

;; [[file:../../../gwp.note::bf455395][bf455395]]
(use-package swiper
  :bind
  (
   ;; ("C-s"   . swiper-isearch)
   ;; ("C-r"   . swiper-isearch-backward)
   ;; ("C-c v p" . ivy-push-view)
   ;; ("C-c v o" . ivy-pop-view)
   ;; ("C-c v ." . ivy-switch-view)
   ("C-x C-b" . +ivy/switch-workspace-buffer)
   ("C-x B" . +ivy/switch-buffer)
   :map swiper-map
   ("M-s" . swiper-isearch-toggle)
   :map isearch-mode-map
   ("M-s" . swiper-isearch-toggle)
   :map ctl-x-4-map
   ("C-s" . ivy-push-view)
   ))
;; bf455395 ends here

;; [[file:../../../gwp.note::b23f833f][b23f833f]]
(defun gwp::swiper-from-clipboard (prefix)
  "从clipboard取词来搜索"
  (interactive "P")
  (let ((keyword
         (simpleclip-get-contents)))
    (swiper-isearch keyword)))
;; b23f833f ends here

;; [[file:../../../gwp.note::f75f80bd][f75f80bd]]
(setq show-trailing-whitespace t)

;; 删除多余空行, 仅保留一行
(map! "C-x C-o" #'delete-blank-lines)

(gwp::text-edit-def
 "C-o" #'cycle-spacing
 "C-c C-o" #'delete-blank-lines
 )
;; f75f80bd ends here

;; [[file:../../../gwp.note::24325443][24325443]]
(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)
(add-hook 'term-exec-hook   'with-editor-export-editor)
(add-hook 'vterm-mode-hook  'with-editor-export-editor)

;; 进入 insert 编辑模式
(add-hook 'with-editor-mode-hook 'meow-insert-mode)
;; 24325443 ends here

;; [[file:../../../gwp.note::fdbbe28d][fdbbe28d]]
;; 中端粘贴时以当前编辑位置为准, 而非鼠标光标所在位置.
(setq mouse-yank-at-point t)

;;;###autoload
(defun gwp::move-mouse-position-to-point ()
  "将鼠标移动到当前编辑位置"
  (interactive)
  (let* (
         (xy (posn-actual-col-row (posn-at-point (point))))
         (x (+ (car xy) (window-left-column)))
         (y (+ (cdr xy) (window-top-line)))
         )
    (set-mouse-position (selected-frame) x y)))
;; fdbbe28d ends here
