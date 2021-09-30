;; [[file:../../doom.note::*completion][completion:1]]
;; Use hippie-expand instead of dabbrev-expand
;; (global-set-key (kbd "M-/") #'dabbrev-expand)
(global-set-key (kbd "M-/") #'hippie-expand)
;; the same behavior as the original `dabbrev-expand'
(setq hippie-expand-dabbrev-skip-space t)

;; adjust the list of functions that hippie-expand will try
(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev-visible      ; first try the expansions from the currently visible parts
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-line
        try-expand-dabbrev-from-kill
        ;; try-expand-all-abbrevs
        ;; try-expand-list
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        ))

(after! company
  (setq company-idle-delay 1.5
        company-minimum-prefix-length 2))

;; 2021-09-19: 没用起来
;; ;; https://github.com/redguardtoo/company-ctags
;; (use-package company-ctags
;;   :after (company)
;;   :init
;;   (setq company-ctags-everywhere t
;;         company-ctags-fuzzy-match-p t)
;;   :config
;;   (company-ctags-auto-setup)
;;   (message "Init company-ctags"))

;; ;; for rust completion
;; ;; https://github.com/dan-t/rusty-tags
;; (setq company-ctags-tags-file-name "rusty-tags.emacs")
;; completion:1 ends here

;; [[file:../../doom.note::*keyfreq][keyfreq:1]]
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
;; keyfreq:1 ends here

;; [[file:../../doom.note::*auto-save][auto-save:1]]
(setq
 ;; doom里已默认为true
 auto-save-default t
 ;; 默认为5秒. 这里改大一些, 避免编辑时自动保存太快, 光标前的空格被吞掉
 auto-save-visited-interval 30)

;; 自动保存至当前文件名, 而临时文件
(auto-save-visited-mode +1)
;; auto-save:1 ends here

;; [[file:../../doom.note::*ctrl-z][ctrl-z:1]]
(after! evil
  ;; 如何误入evil-emacs-state, 按ESC返回normal state
  (define-key evil-emacs-state-map [escape] 'evil-normal-state)
  ;; (map! :nvim "C-z" #'undo)
  )
;; ctrl-z:1 ends here

;; [[file:../../doom.note::*smartparens][smartparens:1]]
(map! :leader
      :nvi
      "DEL" #'sp-backward-kill-sexp     ; BACKSPACE
      [deletechar] #'sp-kill-sexp       ; DELETE
      [right] #'sp-forward-slurp-sexp   ; Array right ==>
      [left]  #'sp-backward-slurp-sexp  ; Array left  <==
      "C-k"   #'sp-unwrap-sexp
      )
;; smartparens:1 ends here

;; [[file:../../doom.note::*expand region][expand region:1]]
(use-package expand-region :after evil :config
  (map! :leader :v "v"
        (function er/expand-region)))
;; expand region:1 ends here

;; [[file:../../doom.note::*doom tuning][doom tuning:1]]
(setq evil-want-fine-undo t)
;; doom tuning:1 ends here

;; [[file:../../doom.note::*better jumper][better jumper:1]]
(map! :nm
      [M-mouse-4] #'better-jumper-jump-backward
      [M-mouse-5] #'better-jumper-jump-forward
      )
;; better jumper:1 ends here

;; [[file:../../doom.note::*insert date][insert date:1]]
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

;; [[file:../../doom.note::*white space][white space:1]]
(setq show-trailing-whitespace t)
(global-set-key (kbd "<f5> SPC") 'delete-trailing-whitespace)
;; make sure this always work
(global-set-key (kbd "C-x C-o") 'delete-blank-lines)
;; white space:1 ends here

;; [[file:../../doom.note::*join next line][join next line:1]]
(after! evil
  (advice-remove #'evil-join #'+evil-join-a)
  )
;; join next line:1 ends here

;; [[file:../../doom.note::*disable mouse][disable mouse:1]]
;; https://endlessparentheses.com/disable-mouse-only-inside-emacs.html
(define-minor-mode disable-mouse-mode
  "A minor-mode that disables all mouse keybinds."
  :global t
  :lighter " 🐭"
  :keymap (make-sparse-keymap))

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
          (vector (intern k)) #'ignore)))))

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
;; disable mouse:1 ends here

;; [[file:../../doom.note::*advanced selection][advanced selection:1]]
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

;; [[file:../../doom.note::*multiedit][multiedit:1]]
(use-package evil-multiedit
  :after evil
  :config
  (setq evil-multiedit-follow-matches t)
  )
;; multiedit:1 ends here

;; [[file:../../doom.note::*bibtex][bibtex:1]]
(setq bibtex-completion-bibliography
      '("~/Data/zotero/my.bib"))
(setq bibtex-completion-pdf-field "file")
(setq bibtex-completion-additional-search-fields '(keywords annotation note))
;; bibtex:1 ends here

;; [[file:../../doom.note::*find notes][find notes:1]]
(defun gwp/find-file-in-notes ()
  "Find a file under `~/.cache/notes', recursively."
  (interactive) (doom-project-find-file "~/.cache/notes"))
;; find notes:1 ends here

;; [[file:../../doom.note::*window][window:1]]
(defhydra gwp/hydra-resize-window ()
  "resize-window"
  ("h" evil-window-decrease-width "decrease width")
  ("j" evil-window-decrease-height "decrease height")
  ("k" evil-window-increase-height "increase height")
  ("l" evil-window-increase-width "increase width")
  ("q" nil "quit")
  )
;; window:1 ends here

;; [[file:../../doom.note::*select text][select text:1]]
(defhydra gwp/hydra-select-text ()
  "select text"
  ("p" er/mark-paragraph "select paragraph")
  ("c" er/mark-org-code-block "select org-code block")
  ("b" gwp/select-none-blank-text "select non blank")
  ("t" gwp/select-text-in-quote "select quoted")
  ("q" nil "quit")
  )
;; select text:1 ends here

;; [[file:../../doom.note::*parens][parens:1]]
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
;; parens:1 ends here

;; [[file:../../doom.note::*bindings][bindings:1]]
(map! :leader
      (:prefix-map ("d" . "hydra")
       :desc "resize window" "w" #'gwp/hydra-resize-window/body
       :desc "select text"   "s" #'gwp/hydra-select-text/body
       :desc "smart parents" "p" #'gwp/hydra-smartparens/body
       ))
;; bindings:1 ends here
