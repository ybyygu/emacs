;; [[file:../../doom.note::*修改 frame 标题 方便 gnome-shell 桌面切换][修改 frame 标题 方便 gnome-shell 桌面切换:1]]
;; workspace@buffer-name: ~/foo/bar
(setq frame-title-format
      '((:eval (+workspace-current-name)) ;
        " | %b : "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))))
        ))
;; 修改 frame 标题 方便 gnome-shell 桌面切换:1 ends here

;; [[file:../../doom.note::bfacbb8e][bfacbb8e]]
(use-package! golden-ratio
  :config
  (map! :map evil-window-map
        "z" #'golden-ratio))
;; bfacbb8e ends here

;; [[file:../../doom.note::9f0e3550][9f0e3550]]
(map! :map evil-window-map
      "1"       #'doom/window-maximize-buffer
      "o"       #'doom/window-maximize-buffer ;show "only"
      "t"       #'doom/window-maximize-vertically ;show top
      )
;; 9f0e3550 ends here

;; [[file:../../doom.note::9a32eb12][9a32eb12]]
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; 9a32eb12 ends here

;; [[file:../../doom.note::19f082d3][19f082d3]]
(use-package! avy
  :config
  (setq avy-all-windows t))

;; 替代 SPC-w-w
;; (global-set-key [remap evil-window-next] #'ace-window)
;; (map! [remap evil-window-next] #'ace-window)

(map! :map evil-window-map
      "w" #'ace-window
      "r" #'ace-swap-window             ; rotate
      "c" #'ace-delete-window           ; close
      "f" #'tear-off-window             ; 类似于firefox中的标签变窗口 (float)
      )
;; 19f082d3 ends here

;; [[file:../../doom.note::*弹出窗口][弹出窗口:1]]
(map! :i "C-`" #'+popup/toggle)
;; 弹出窗口:1 ends here

;; [[file:../../doom.note::*窗口大小][窗口大小:1]]
(map! :nvi
      [C-M-mouse-4] #'evil-window-increase-width
      [C-M-mouse-5] #'evil-window-decrease-width
      )
;; 窗口大小:1 ends here

;; [[file:../../doom.note::*窗口大小][窗口大小:2]]
(setq split-width-threshold 200)        ; default is 160
;; 窗口大小:2 ends here

;; [[file:../../doom.note::f07dc327][f07dc327]]
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

;; [[file:../../doom.note::19e08aef][19e08aef]]
(defun gwp/display-current-buffer-other-frame ()
  "display current buffer on other frame"
  (interactive)
  (display-buffer-other-frame (current-buffer)))
;; 19e08aef ends here

;; [[file:../../doom.note::15078428][15078428]]
(use-package! pyim
  :config
  (setq default-input-method "pyim")

  ;; 我使用五笔
  (use-package! pyim-wbdict
    :config (pyim-wbdict-v98-enable))
  (setq pyim-default-scheme 'wubi)

  ;; 启用拼音大词库，方便忘词用拼音反查
  (use-package pyim-basedict
    :config (pyim-basedict-enable))
  ;; 如果用户在使用五笔输入法的过程中，忘记了某个字的五笔码，可以按 TAB(F2-TAB)键临时切换到辅助输入法来输入，选词完成之后自动退出。
  (setq pyim-assistant-scheme 'quanpin)

  ;; 全角半角
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 不使用中文标点, 如需输入可切换至fcitx.
  ;; (delete '("/" "、")  pyim-punctuation-dict)
  ;; (add-to-list 'pyim-punctuation-dict '("\\" "、"))
  (setq pyim-punctuation-dict nil)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  ;; (setq-default pyim-english-input-switch-functions
  ;;               '(
  ;;                 ;; pyim-probe-dynamic-english
  ;;                 pyim-probe-isearch-mode
  ;;                 pyim-probe-program-mode
  ;;                 pyim-probe-org-structure-template
  ;;                 ))

  ;; 与 pyim-probe-dynamic-english 配合，方便切换至中文模式
  :bind
  (
   ;; ("<f2> SPC" . pyim-convert-string-at-point)
   ;; ("M-SPC" . toggle-input-method)
   (:map pyim-mode-map
    ;; ("<f2> TAB" . pyim-toggle-assistant-scheme)
    ("/" . pyim-toggle-assistant-scheme)
    ("_" . pyim-toggle-input-ascii)
    ("\\" . pyim-toggle-input-ascii)
    )))
;; 15078428 ends here

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

;; [[file:../../doom.note::*theme][theme:1]]
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

;; [[file:../../doom.note::*line number][line number:1]]
(setq display-line-numbers-type nil)
;; line number:1 ends here

;; [[file:../../doom.note::6013493c][6013493c]]
;; View images inside Emacs
(auto-image-file-mode t)

;; set line space wider than default
(setq-default line-spacing 4)
;; 6013493c ends here
