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

(use-package! citre
  :defer t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  (require 'ob-tangle)
  (map! :leader
        (:prefix-map ("j" . "jump")
         :desc "jump to org src"                              "o" #'org-babel-tangle-jump-to-org
         (:prefix-map ("c" . "citre")
          :desc "citre jump to definition"                    "d" #'citre-jump
          :desc "citre jump back"                             "b" #'citre-jump-back
          :desc "citre peek"                                  "p" #'citre-peek
          ))))
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

;; [[file:../../doom.note::6ded2bf1][6ded2bf1]]
(map! :nm
      [M-mouse-4] #'better-jumper-jump-backward
      [M-mouse-5] #'better-jumper-jump-forward)

(map! :leader
      (:prefix-map ("j" . "jump")
       (:prefix-map ("a" . "avy")
        :desc "Search and jump"                 "s" #'avy-goto-char-2
        :desc "jump to line"                    "l" #'avy-goto-line
        )))
;; 6ded2bf1 ends here

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

;; [[file:../../doom.note::2286a7d2][2286a7d2]]
(after! evil
  (advice-remove #'evil-join #'+evil-join-a)
  )
;; 2286a7d2 ends here

;; [[file:../../doom.note::7628d03d][7628d03d]]
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

;; [[file:../../doom.note::be09bc09][be09bc09]]
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
    ("e" "mark email" er/mark-email)
    ]]
  )
;; be09bc09 ends here

;; [[file:../../doom.note::*advanced selection][advanced selection:2]]
;; https://stackoverflow.com/a/22418983
(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

; between pipe characters:
(define-and-bind-text-object "|" "|" "|")
(define-and-bind-text-object "d" "\"" "\"")
;; advanced selection:2 ends here

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
      (:prefix-map ("d" . "mine")
       :desc "select text"   "s" #'gwp/advanced-selection
       :desc "resize window" "w" #'gwp/hydra-resize-window/body
       :desc "smart parents" "p" #'gwp/hydra-smartparens/body
       ))
;; bindings:1 ends here
