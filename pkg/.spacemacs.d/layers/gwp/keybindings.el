;; [[file:~/Install/configs/spacemacs/config.note::aeb269bf-d17f-4d46-9f32-d31d522b72e0][aeb269bf-d17f-4d46-9f32-d31d522b72e0]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  File:       ~/.spacemacs.d/layers/gwp/keybindings.el
;;  Created:    <2017-10-09 Mon>
;;  UPDATED:    <2017-10-27 Fri 19:35>
;;  Platform:   Emacs (Spacemacs)
;;  Author:     Wenping Guo <ybyygu@gmail.com>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aeb269bf-d17f-4d46-9f32-d31d522b72e0 ends here

;; [[file:~/Install/configs/spacemacs/config.note::3631de70-8cb6-494d-bb97-c8d6100f7993][3631de70-8cb6-494d-bb97-c8d6100f7993]]
;; tangle blocks for current file at point
;; http://stackoverflow.com/questions/28727190/org-babel-tangle-only-one-code-block
;; call org-babel-tangle with C-u C-u
(defun gwp/org-babel-tangle-block()
  (interactive)
  (let ((current-prefix-arg '(16)))
    (call-interactively 'org-babel-tangle)
    )
  )

(eval-after-load "org"
  '(progn
     (org-defkey org-mode-map [(meta return)] 'org-meta-return)
     ;; use o as org global bindings
     (spacemacs/set-leader-keys
       "oa" 'org-agenda
       "oo" 'org-attach
       "oc" 'org-capture
       "ol" 'org-store-link
       "ob" 'gwp/org-babel-tangle-block
       "oj" 'org-babel-tangle-jump-to-org
       )
     )
  )
;; 3631de70-8cb6-494d-bb97-c8d6100f7993 ends here

;; [[file:~/Install/configs/spacemacs/config.note::6d5783e3-e1d2-4818-b2dd-ba60a4891505][6d5783e3-e1d2-4818-b2dd-ba60a4891505]]
(defun gwp/insert-date (arg)
  "Insert date at point. With prefix argument, insert date and time."
  (interactive "P")
  (insert (format-time-string "%Y-%m-%d"))
  (when arg
    (insert (format-time-string " %H:%M"))
    )
  )

;; make it easier to update time-stamp
;; (global-set-key (kbd "C-c i") 'gwp/insert-date)
(define-key evil-hybrid-state-map (kbd "C-c i") 'gwp/insert-date)
;; 6d5783e3-e1d2-4818-b2dd-ba60a4891505 ends here

;; [[file:~/Install/configs/spacemacs/config.note::85eb0f16-11ae-4ff5-b161-c4646e1397a4][85eb0f16-11ae-4ff5-b161-c4646e1397a4]]
(setq show-trailing-whitespace t)
(global-set-key (kbd "<f5> SPC") 'delete-trailing-whitespace)
;; make sure this always work
(global-set-key (kbd "C-x C-o") 'delete-blank-lines)
;; 85eb0f16-11ae-4ff5-b161-c4646e1397a4 ends here

;; [[file:~/Install/configs/spacemacs/config.note::33688115-a6e5-444d-a4da-d2bbd95cfdf6][33688115-a6e5-444d-a4da-d2bbd95cfdf6]]
(defvar my-skeleton-pair-alist
  '((?\) . ?\()
    (?\] . ?\[)
    (?} . ?{)
    (?" . ?")
    )
  )

(defun my-skeleton-pair-end (arg)
  "Skip the char if it is an ending, otherwise insert it."
  (interactive "*p")
  (let ((char last-command-char))
    (if (and (assq char my-skeleton-pair-alist)
             (eq char (following-char)))
        (forward-char)
      (self-insert-command (prefix-numeric-value arg))
      )
    )
  )

(global-set-key (kbd "<f5> (") 'insert-pair)
(global-set-key (kbd "<f5> <") 'insert-pair)
(global-set-key (kbd "<f5> [") 'insert-pair)
(global-set-key (kbd "<f5> {") 'insert-pair)
(global-set-key (kbd "<f5> \"") 'insert-pair)
(global-set-key (kbd "<f5> '") 'insert-pair)
(global-set-key (kbd "<f5> C-d") 'delete-pair)
;; 33688115-a6e5-444d-a4da-d2bbd95cfdf6 ends here

;; [[file:~/Install/configs/spacemacs/config.note::30ff64af-743a-4ae0-8642-025fa1848196][30ff64af-743a-4ae0-8642-025fa1848196]]
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

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun gwp/extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

;; (global-set-key (kbd "<f5> v") 'gwp/extend-selection)

(defun gwp/select-text-in-quote ()
"Select text between the nearest left and right delimiters.
Delimiters are paired characters: ()[]<>«»“”‘’「」, including \"\"."
 (interactive)
 (let (b1 b2)
   (skip-chars-backward "^<>(“{[「«\"‘")
   (setq b1 (point))
   (skip-chars-forward "^<>)”}]」»\"’")
   (setq b2 (point))
   (set-mark b1)
   )
 )

(defun gwp/select-none-blank-text ()
"Select none blank chars near the point in current line"
 (interactive)
 (let (b1 b2)
   (skip-chars-backward "^ \n")
   (setq b1 (point))
   (skip-chars-forward "^ \n")
   (setq b2 (point))
   (set-mark b1)
   )
 )

(defun gwp/select-word ()
"Select none blank chars near the point in current line"
 (interactive)
 (let (b1 b2)
   (backward-word)
   (setq b1 (point))
   (forward-word)
   (setq b2 (point))
   (set-mark b1)
   )
 )

(defun gwp/select-line ()
"Select current line"
 (interactive)
 (let (b1 b2)
   (move-beginning-of-line nil)
   (setq b1 (point))
   (move-end-of-line nil)
   (setq b2 (point))
   (set-mark b1)
   )
 )

;; (global-set-key (kbd "M-*") 'select-text-in-quote)
;; (global-set-key (kbd "M-6") 'select-line)
;; (global-set-key (kbd "M-4") 'select-word)
(global-set-key (kbd "M-5") 'gwp/select-none-blank-text)

;; https://github.com/magnars/expand-region.el
;; (require 'expand-region)
;; (global-set-key (kbd "M-4") 'er/expand-region)
;; 30ff64af-743a-4ae0-8642-025fa1848196 ends here

;; [[file:~/Install/configs/spacemacs/config.note::4ea1e496-e909-40a7-bbf7-f4ced06210fd][4ea1e496-e909-40a7-bbf7-f4ced06210fd]]
;; (global-set-key (kbd "C-l") 'goto-line)

;; make it easier to paste things
;; (define-key evil-insert-state-map (kbd "C-v") 'yank)
;; becasue I use hybrid mode
(define-key evil-hybrid-state-map (kbd "C-v") 'yank)

;; make it easier to undo
(define-key evil-hybrid-state-map (kbd "C-z") 'undo-tree-undo)
(define-key evil-hybrid-state-map (kbd "M-u") 'undo-tree-undo)

;; remap C-a
(global-set-key [remap org-beginning-of-line]
                'evil-first-non-blank-of-visual-line)
;; remap C-e
(global-set-key [remap evil-scroll-line-down]
                'evil-end-of-visual-line)
;; default binding: C-x 4 0
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)
;; 4ea1e496-e909-40a7-bbf7-f4ced06210fd ends here

;; [[file:~/Install/configs/spacemacs/config.note::eae47e9e-63d5-47cd-a2a0-542299176edf][eae47e9e-63d5-47cd-a2a0-542299176edf]]
; (starter-kit-install-if-needed 'line-comment-banner)
; (require 'line-comment-banner)
; (global-set-key (kbd "<f5> ;") 'line-comment-banner)

;; stolen from: http://www.emacswiki.org/emacs/CommentingCode
;; (defun comment-dwim-line (&optional arg)
;;   "Replacement for the comment-dwim command.
;;   If no region is selected and current line is not blank and we are not at the end of the line,
;;   then comment current line.
;;   Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
;;     (interactive "*P")
;;     (comment-normalize-vars)
;;     (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
;;     (comment-or-uncomment-region (line-beginning-position) (line-end-position))
;;       (comment-dwim arg)))
;; (global-set-key "\M-;" 'comment-dwim-line)
;; eae47e9e-63d5-47cd-a2a0-542299176edf ends here
