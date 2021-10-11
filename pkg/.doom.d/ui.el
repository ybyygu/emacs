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
      "r" #'ace-swap-window   ; rotate
      "c" #'ace-delete-window ; close
      "f" #'tear-off-window   ; 类似于firefox中的标签变窗口 (float, move to new frame)
      "F" #'follow-mode       ; 同步滚动窗口, 可用于双窗口内容对比等
      )
;; 19f082d3 ends here

;; [[file:../../doom.note::*弹出窗口][弹出窗口:1]]
(map! :i "C-`" #'+popup/toggle)
;; 弹出窗口:1 ends here

;; [[file:../../doom.note::032e71c0][032e71c0]]
(map! :nvi
      [C-M-mouse-4] #'evil-window-increase-width
      [C-M-mouse-5] #'evil-window-decrease-width
      )
;; 032e71c0 ends here

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

;; [[file:../../doom.note::155b72b3][155b72b3]]
(use-package! rime
  :custom
  (default-input-method "rime")
  :config
  (setq rime-user-data-dir "~/.local/share/fcitx5/rime")
  ;;; support shift-l, shift-r, control-l, control-r
  (setq rime-inline-ascii-trigger 'shift-l)
  ;; 临时英文中阻止标点直接上屏
  (setq rime-inline-ascii-holder ?x)      ; Any single character that not trigger auto commit
  ;; 添加C-.快捷键, 方便切换中英文标点(需要在rime输入时有效)
  (setq rime-translate-keybindings
        '("C-f" "C-b" "C-n" "C-p" "C-g" "C-."))
  ;; 这里需要与fcitx配合: 去掉GTK_IM_MODULE, XMODIFIERS等FCITX输入法设置变量.
  (map! :ni "C-SPC" 'toggle-input-method)
  ;; 在输入且有码上屏的状态下, 可用TAB临时切换英文.
  (map! :map rime-active-mode-map [tab] 'rime-inline-ascii)
  ;; (setq rime-posframe-properties
  ;;       (list :background-color "#333333"
  ;;             :foreground-color "#dcdccc"
  ;;             :font "WenQuanYi Micro Hei Mono-14"
  ;;             :internal-border-width 10))
  ;; (setq default-input-method "rime"
  ;;       rime-show-candidate 'posframe)
  )
;; 155b72b3 ends here

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
