;; [[file:../../../../../doom.note::a6f83332][a6f83332]]
;; 默认不要显示折行
(global-visual-line-mode -1)

(defun gwp::turn-off-wrap-long-line()
  (global-visual-line-mode -1))
(add-hook 'org-mode-hook 'gwp::turn-off-wrap-long-line)
;; a6f83332 ends here

;; [[file:../../../../../doom.note::4e3b4060][4e3b4060]]
(blink-cursor-mode 1)
;; 4e3b4060 ends here

;; [[file:../../../../../doom.note::885c9fa9][885c9fa9]]
;; 方便绑定到 SPC-t-l
;;
;; 仅切换relative和none两种状态, doom的要切三种
(defun gwp::toggle-line-numbers ()
  (interactive)
  (if display-line-numbers
      (setq display-line-numbers 'nil)
    (setq display-line-numbers 'relative)))

(defun gwp::display-line-numbers ()
  (setq display-line-numbers 'relative))

;; NOTE: org-mode在折叠状态下, 相对行号显示的是实际数目, 而非折叠后的, 这对编辑操作没多大帮助了.
;; ;; (add-hook 'org-mode-hook #'gwp::display-line-numbers)
(add-hook 'org-src-mode-hook #'gwp::display-line-numbers)
;; (add-hook 'prog-mode-hook #'gwp::display-line-numbers)
(add-hook 'rust-mode-hook #'gwp::display-line-numbers)

;; 全局设置
;; (setq display-line-numbers-type 'relative)
;; 885c9fa9 ends here

;; [[file:../../../../../doom.note::*修改 frame 标题 方便 gnome-shell 桌面切换][修改 frame 标题 方便 gnome-shell 桌面切换:1]]
;; workspace@buffer-name: ~/foo/bar
(setq frame-title-format
      '((:eval (+workspace-current-name)) ;
        " | %b : "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))))
        ))
;; 修改 frame 标题 方便 gnome-shell 桌面切换:1 ends here

;; [[file:../../../../../doom.note::bfacbb8e][bfacbb8e]]
(use-package! golden-ratio
  :config
  (map! :map evil-window-map
        "z" #'golden-ratio))
;; bfacbb8e ends here

;; [[file:../../../../../doom.note::9f0e3550][9f0e3550]]
(map! :map evil-window-map
      "1"       #'doom/window-maximize-buffer
      "o"       #'doom/window-maximize-buffer ;show "only"
      "t"       #'doom/window-maximize-vertically ;show top
      )
;; 9f0e3550 ends here

;; [[file:../../../../../doom.note::9a32eb12][9a32eb12]]
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; 9a32eb12 ends here

;; [[file:../../../../../doom.note::19f082d3][19f082d3]]
(use-package! avy
  :config
  (setq avy-all-windows t))

;; 替代 SPC-w-w
;; (global-set-key [remap evil-window-next] #'ace-window)
;; (map! [remap evil-window-next] #'ace-window)

(map! :map evil-window-map
      "w" #'ace-window
      "r" #'ace-swap-window   ; rotate
      "c" #'ace-delete-window ; close
      "f" #'tear-off-window   ; 类似于firefox中的标签变窗口 (float, move to new frame)
      "F" #'follow-mode       ; 同步滚动窗口, 可用于双窗口内容对比等
      )
;; 19f082d3 ends here

;; [[file:../../../../../doom.note::*弹出窗口][弹出窗口:1]]
(map! :i "C-`" #'+popup/toggle)
;; 弹出窗口:1 ends here

;; [[file:../../../../../doom.note::032e71c0][032e71c0]]
(map! :nvi
      [C-M-mouse-4] #'evil-window-increase-width
      [C-M-mouse-5] #'evil-window-decrease-width
      )
;; 032e71c0 ends here

;; [[file:../../../../../doom.note::*窗口大小][窗口大小:2]]
(setq split-width-threshold 200)        ; default is 160
;; 窗口大小:2 ends here

;; [[file:../../../../../doom.note::f07dc327][f07dc327]]
(defhydra gwp/adjust-window-size ()
  "resize-window"
  ("h" evil-window-decrease-width "decrease width")
  ("j" evil-window-decrease-height "decrease height")
  ("k" evil-window-increase-height "increase height")
  ("l" evil-window-increase-width "increase width")
  ("q" nil "quit")
  )

(map! :map evil-window-map
      "a"       #'gwp/adjust-window-size/body ; adjust
      )
;; f07dc327 ends here

;; [[file:../../../../../doom.note::19e08aef][19e08aef]]
(defun gwp::display-current-buffer-other-frame ()
  "在其它 frame 中显式当前 buffer"
  (interactive)
  (switch-to-buffer-other-frame (current-buffer)))
;; 19e08aef ends here

;; [[file:../../../../../doom.note::bf66c13f][bf66c13f]]
(require 'ivy)
(ivy-set-actions
 ;; 以下会覆盖默认定义的, 所以需要补回
 'ivy-switch-buffer
 '(
   ("f" ivy--find-file-action "find file")
   ("j" switch-to-buffer-other-window "other window")
   ("k" ivy--kill-buffer-action "kill")
   ("r" ivy--rename-buffer-action "rename")
   ("x" counsel-open-buffer-file-externally "open externally")
   ("f" switch-to-buffer-other-frame "other frame") ; 默认没有
   ("t" switch-to-buffer-other-tab "other tab")     ; 默认没有
   ))
;; bf66c13f ends here

;; [[file:../../../../../doom.note::a207c706][a207c706]]
(use-package burly
  :config
  (map! :map ctl-x-5-map "s" #'burly-bookmark-windows))
;; a207c706 ends here

;; [[file:../../../../../doom.note::155b72b3][155b72b3]]
(use-package! rime
  :custom
  (default-input-method "rime")
  :config
  (setq rime-user-data-dir "~/.local/share/fcitx5/rime")
  ;; 这个设置与rime的一致, 不然emacs中的inline ascii无法生效
  ;;; support shift-l, shift-r, control-l, control-r
  (setq rime-inline-ascii-trigger 'shift-l)
  ;; 临时英文中阻止标点直接上屏
  (setq rime-inline-ascii-holder ?x)      ; Any single character that not trigger auto commit
  ;; 添加C-.快捷键, 方便切换中英文标点(需要在rime输入时有效)
  (setq rime-translate-keybindings
        '("C-f" "C-b" "C-n" "C-p" "C-g" "C-."))
  ;; 在输入且有码上屏的状态下, 可用TAB临时切换英文.
  (map! :map rime-active-mode-map :after ivy [tab] 'rime-inline-ascii)
  ;; NOTE: 以下有时会让emacs crash
  (setq rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :internal-border-width 10))
  (setq default-input-method "rime"
        rime-show-candidate 'posframe)

  ;; 自动进入英文录入状态, 相当于直接输入英文
  (setq rime-disable-predicates
        '(
          evil-normal-state-p
          ;; 首字母为是英文字母时进入英文模式
          rime-predicate-after-alphabet-char-p
          ;; 将要输入的为大写字母时
          rime-predicate-current-uppercase-letter-p
          ;; 在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
          rime-predicate-prog-in-code-p
          ;; 在 (La)TeX 数学环境中或者输入 (La)TeX 命令时
          rime-predicate-tex-math-or-command-p
          ;; 在中文字符且有空格之后
          ;; rime-predicate-space-after-cc-p
          ))
  ;; 进入连续英文状态, 空格或回车键上屏
  (setq rime-inline-predicates
        '(
          rime-predicate-space-after-cc-p
          )))
;; 155b72b3 ends here

;; [[file:../../../../../doom.note::b254d4bc][b254d4bc]]
;; https://github.com/jadestrong/dotfiles/blob/master/home/.doom.d/modules/input/chinese2/config.el
(defun gwp::rime-convert-string-at-point (&optional return-cregexp)
  "将光标前的字符串转换为中文."
  (interactive "P")
  (let ((string
         (if mark-active
             (buffer-substring-no-properties
              (region-beginning) (region-end))
           (buffer-substring-no-properties
            (point) (max (line-beginning-position) (- (point) 80)))))
        code
        length)
    (cond ((string-match "\\([a-z]+\\) *$" string)
           (setq code (match-string 0 string))
           (setq length (length code))
           (setq code (replace-regexp-in-string " +" "" code))
           (if mark-active
               (delete-region (region-beginning) (region-end))
             (when (> length 0)
               (delete-char (- 0 length))))
           (when (> length 0)
             (setq unread-command-events
                   (append (listify-key-sequence code)
                           unread-command-events))))
          (t (message "`rime-convert-string-at-point' did nothing.")))))
;; b254d4bc ends here

;; [[file:../../../../../doom.note::37aafacc][37aafacc]]
(defun gwp::rime-toggle-input ()
  "切换 rime 中文输入状态."
  (interactive)

  (let ((input-method "rime"))
    (toggle-input-method)
    ;; evil 下, 直接进入 insert 模式
    (when (rime-predicate-evil-mode-p)
      (if (= (+ 1 (point)) (line-end-position))
          (evil-append 1)
        (evil-insert 1)))

    ;; 进入 rime 输入状态后, 把误按的字符转换中文
    (when (string= current-input-method input-method)
      (gwp::rime-convert-string-at-point))

    ;; 提示当前输入状态, 比看图标更醒目
    (if current-input-method
        (message "IME on")
      (message "IME off"))))
;; 37aafacc ends here

;; [[file:../../../../../doom.note::c457613c][c457613c]]
;; 这里需要与fcitx配合: 去掉GTK_IM_MODULE, XMODIFIERS等FCITX输入法设置变量.
(map! :nieg "C-SPC" 'gwp::rime-toggle-input)
;; (map! :nieg "C-SPC" 'gwp::rime-force-enable)
;; NOTE: 因为与ivy的默认绑定有冲突, minibuffer下不能切换
;; ivy-call-and-recenter
;; 2021-10-13: 直接map不太有效, 时灵不灵的
;; (map! :map ivy-minibuffer-map "C-SPC" #'toggle-input-method)
;; NOTE: 可用M-RET来预览选中条目, 而不退出ivy窗口
(map! :after ivy :map ivy-minibuffer-map [remap ivy-call-and-recenter] 'toggle-input-method)

;; 将光标英文字符转化为中文录入
(map! :map rime-mode-map "M-i" #'gwp::rime-convert-string-at-point)
;; 在自动英文模式下强制进入中文状态
(map! :map rime-mode-map "C-i" #'rime-force-enable)
;; c457613c ends here

;; 2021-08-25: 留着, 但暂时用不上
;; https://emacs-china.org/t/doom-emacs/10390
(defun gwp/set-fonts()
  (interactive)
  (if (display-graphic-p)
      (progn
        ;; english font
        ;; (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "Monaco" 16)) ;; 11 13 17 19 23
        ;; (setq doom-font (font-spec :family "Monaco" :size 16))
        ;; (setq doom-font (font-spec :family "Monaco"))
        ;; chinese font
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Adobe Heiti Std")))) ;; 14 16 20 22 28
    ))

;; 2021-08-25: 不需要单独设置了
;; org-mode表格中文混排对齐
;; (after! org
;;   ;; (call-interactively 'gwp/set-fonts)
;;   (custom-set-faces!
;;     `(org-table :family "Ubuntu Mono")
;;     )
;;   )

;; for doom-emacs only
;; https://emacs-china.org/t/emacs/15676/20
;; https://github.com/ztlevi/doom-config/blob/main/%2Bui.el
(when (display-graphic-p)
  (setq user-font
        (cond
         ((find-font (font-spec :name  "Sarasa Fixed SC")) "Sarasa Fixed SC")
         ((find-font (font-spec :name  "Iosevka")) "Iosevka")
         ((find-font (font-spec :name  "Inconsolata Nerd Font")) "Inconsolata Nerd Font")
         ((find-font (font-spec :name  "Ubuntu Mono")) "Ubuntu Mono")))
  ;; (setq resolution-factor (eval (/ (x-display-pixel-height) 1080)))
  (setq resolution-factor 2)
  (setq ideal-font-size (eval (* 15 resolution-factor)))
  (setq big-font-size (eval (* 18 resolution-factor)))
  (setq doom-font (font-spec :family user-font :size ideal-font-size)
        ;; doom-serif-font (font-spec :family user-font)
        doom-variable-pitch-font (font-spec :family user-font)
        ;; doom-unicode-font (font-spec :family user-font)
        doom-big-font (font-spec :family user-font :size big-font-size))
  )

;; [[file:../../../../../doom.note::*theme][theme:1]]
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-acario-light)
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-material)
(setq doom-theme 'doom-nova)
;; (setq doom-theme 'doom-vibrant)
;; (load-theme 'material t)
(custom-set-faces!
  '(org-todo :foreground "yellow" :weight bold :background "#263238")
  '(org-done :foreground "yellow" :weight bold :background "#263238")
  '(org-table :foreground "#e3f2fd")
  '(hl-line :background "#37474f")
  '(org-level-1 :foreground "#e3f2fd" :height 1.1 :background nil :weight bold :box nil)
  '(org-level-2 :foreground "#e3f2fd" :height 1.0 :background nil :weight normal :box nil)
  '(org-headline-done :foreground "gray" :weight normal)
  )

;; 当前行
(custom-set-faces!
  '(hl-line :background "#37474f")
  '(solaire-hl-line-face :background "#37474f")
  '(region :background "#555555")
  )
;; theme:1 ends here

;; [[file:../../../../../doom.note::fae9a6ea][fae9a6ea]]
;; symbol-overlay
;;;  a highlight-symbol replacement.
(use-package symbol-overlay
  :config
  ;; 用 transient 不如下面的好. 下面的可以用"."命令来重做上次的操作.
  (general-define-key :prefix-map 'gwp::symbol-overlay-map
                      "h" 'symbol-overlay-put
                      "r" 'symbol-overlay-rename
                      "t" 'symbol-overlay-toggle-in-scope
                      "n" 'symbol-overlay-switch-forward ; 当在高亮的字符外时, 可快速返回.
                      "p" 'symbol-overlay-switch-backward
                      "d" 'symbol-overlay-remove-all
                      "R" 'symbol-overlay-query-replace)
  ;; 等价设置; 备忘
  ;; (setq symbol-overlay-map (make-sparse-keymap))
  ;; (setq gwp::symbol-overlay-map (make-sparse-keymap))
  ;; (define-key gwp::symbol-overlay-map (kbd "h") 'symbol-overlay-put)
  ;; (define-key gwp::symbol-overlay-map (kbd "n") 'symbol-overlay-jump-next)
  ;; (define-key gwp::symbol-overlay-map (kbd "p") 'symbol-overlay-jump-prev)
  ;; (define-key gwp::symbol-overlay-map (kbd "w") 'symbol-overlay-save-symbol)
  ;; (define-key gwp::symbol-overlay-map (kbd "t") 'symbol-overlay-toggle-in-scope)
  ;; (define-key gwp::symbol-overlay-map (kbd "e") 'symbol-overlay-echo-mark)
  ;; (define-key gwp::symbol-overlay-map (kbd "d") 'symbol-overlay-jump-to-definition)
  ;; (define-key gwp::symbol-overlay-map (kbd "s") 'symbol-overlay-isearch-literally)
  ;; (define-key gwp::symbol-overlay-map (kbd "q") 'symbol-overlay-query-replace)
  ;; (define-key gwp::symbol-overlay-map (kbd "r") 'symbol-overlay-rename)
  )
;; fae9a6ea ends here

;; [[file:../../../../../doom.note::6013493c][6013493c]]
;; View images inside Emacs
(auto-image-file-mode t)

;; 相当于行间距
(setq-default line-spacing 4)
;; 6013493c ends here

;; [[file:../../../../../doom.note::79a17a52][79a17a52]]
(defun gwp::switch-to-messages-buffer (&optional arg)
  (interactive "P")
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (if arg
        (switch-to-buffer-other-frame (current-buffer))
      (switch-to-buffer (current-buffer)))))

(map! :leader :desc "switch *Message* buffer" "M" #'gwp::switch-to-messages-buffer)
;; 79a17a52 ends here
