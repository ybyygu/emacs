;; [[file:~/Workspace/Programming/emacs/doom.note::*smartparens][smartparens:1]]
(map! :leader
      :nvi
      "DEL" #'sp-backward-kill-sexp     ; BACKSPACE
      [deletechar] #'sp-kill-sexp       ; DELETE
      [right] #'sp-forward-slurp-sexp   ; Array right ==>
      [left]  #'sp-backward-slurp-sexp  ; Array left  <==
      "C-k"   #'sp-unwrap-sexp
      )
;; smartparens:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*expand region][expand region:1]]
(use-package expand-region :after evil :config
  (map! :leader :v "v"
        (function er/expand-region)))
;; expand region:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*doom tuning][doom tuning:1]]
(setq evil-want-fine-undo t)
;; doom tuning:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*insert date][insert date:1]]
(defun gwp/insert-date (arg)
  "Insert date at point. With prefix argument, insert date and time."
  (interactive "P")
  (insert (format-time-string "%Y-%m-%d"))
  (when arg
    (insert (format-time-string " %H:%M"))
    )
  )

;; make it easier to update time-stamp
(map! :i "C-c i" #'gwp/insert-date)
;; insert date:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*white space][white space:1]]
(setq show-trailing-whitespace t)
(global-set-key (kbd "<f5> SPC") 'delete-trailing-whitespace)
;; make sure this always work
(global-set-key (kbd "C-x C-o") 'delete-blank-lines)
;; white space:1 ends here

;; [[file:~/Workspace/Programming/emacs/doom.note::*TODO advanced selection][advanced selection:1]]
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
;; advanced selection:1 ends here
