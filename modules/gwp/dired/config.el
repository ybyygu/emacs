;; [[file:../../../gwp.note::6be89afb][6be89afb]]
(defun gwp::dired-hook ()
  ;; 高亮当前行, 醒目
  (hl-line-mode 1)
  ;; 不显示隐藏文件
  (dired-omit-mode 1)
  ;; 启用折叠空目录模式
  (dired-collapse-mode 1))

;;;###autoload
(defun gwp::dired-quit-all ()
  "Kill all `dired-mode' buffers."
  (interactive)
  (mapc #'kill-buffer (doom-buffers-in-mode 'dired-mode))
  (message "Killed all dired buffers"))

;;;###autoload
(defun gwp::dired-goto-first ()
  (interactive)
  (goto-char (point-min))
  (dired-hacks-next-file))

;;;###autoload
(defun gwp::dired-goto-last ()
  (interactive)
  (goto-char (point-max))
  (dired-hacks-previous-file))

;;;###autoload
(defun gwp::dired-quit-window ()
  "退出 dired 时, kill 对应的 buffer"
  (interactive)
  ;; 在 split window 情况下, 我希望仅关掉当前窗口, 而非 bury 或 kill 对应的 buffer
  (let* ((same-buffer-in-window (delq (selected-window) (get-buffer-window-list))))
    (if same-buffer-in-window
        (progn
          (message "keep window buffer alive")
          (delete-window))
      ;; kill dired buffer
      (quit-window t))))

;;;###autoload
(defun gwp::dired-find-alternate-file ()
  "访问文件时避免新开多余的 buffer"
  (interactive)
  (cond
   ;; if the same buffer in other window
   ((delq (selected-window) (get-buffer-window-list))
    (dired-find-file))
   (t
    (dired-find-alternate-file))))

;; (defun gwp::dired-up-directory ()
;;   (interactive)
;;   (let* ((dir (dired-current-directory))
;;          (orig (current-buffer))
;;          (up (file-name-directory (directory-file-name dir))))
;;     (or (dired-goto-file (directory-file-name dir))
;;         ;; Only try dired-goto-subdir if buffer has more than one dir.
;;         (and (cdr dired-subdir-alist)
;;              (dired-goto-subdir up))
;;         (progn
;;           (kill-buffer orig)
;;           (dired up)
;;           (dired-goto-file dir)))))

;; https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
;;;###autoload
(defun gwp::dired-up-directory ()
  (interactive)
  (cond
   ;; if the same buffer in other window
   ((delq (selected-window) (get-buffer-window-list))
    (dired-up-directory))
   (t
    (find-alternate-file ".."))))

;;;###autoload
(defun gwp::dired::symbol-link-at-point-to-home ()
  "在 dired 中, 将光前文件软链接到 HOME 下, 方便快速访问(比如在
virtualbox /windows 中)"
  (interactive)

  (if (derived-mode-p 'dired-mode)
      (let ((this-file (dired-get-file-for-visit))
            (target-path "~/00-dired-tmp-entry-point"))
        (make-symbolic-link this-file target-path)
        (message "symlink to: %s" target-path))
    (user-error "not in dired buffer")))

;;;###autoload
(defun gwp::dired-fd ()
  "用于递归搜索文件名, 生成 dired 视图"
  (interactive)
  (require 'fd-dired)
  (let ((args (read-string
               "Run fd (with args and search): "
               fd-dired-input-fd-args
               '(fd-dired-args-history . 1))))
    (fd-dired "." args)))
;; 6be89afb ends here

;; [[file:../../../gwp.note::67caa559][67caa559]]
(use-package dired
  :custom
  ;; 方便多个文件夹文件互动
  ;; Set this variable to non-nil, Dired will try to guess a default
  ;; target directory. This means: if there is a dired buffer
  ;; displayed in the next window, use its current subdir, instead
  ;; of the current subdir of this dired buffer. The target is used
  ;; in the prompt for file copy, rename etc.
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)          ; don't prompt to revert; just do it
  (dired-recursive-copies  'always)
  (dired-recursive-deletes 'top)

  ;; Dired listing switches
  ;;  -a : Do not ignore entries starting with .
  ;;  -l : Use long listing format.
  ;;  -G : Do not print group names like 'users'
  ;;  -h : Human-readable sizes like 1K, 234M, ..
  ;;  -v : Do natural sort .. so the file names starting with . will show up first.
  (dired-listing-switches "-alhvG --group-directories-first") ; default: "-al"

  :config
  (add-hook! 'dired-mode-hook #'gwp::dired-hook)
  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil)
  ;; 安全第一
  (setq delete-by-moving-to-trash t)
  (progn
    ;; 用于在dired中复制当前文件的全路径.
    (defun gwp/dired-copy-file-path()
      (interactive)
      (let ((current-prefix-arg '(0)))
        (call-interactively 'dired-copy-filename-as-kill)))

    ;; 使用BACKSPACE来上一级目录, 使用Ctrl-shift-n来新建目录(默认为"+")
    (map! :map dired-mode-map
          "q"     #'gwp::dired-quit-window
          "j"     #'dired-hacks-next-file     ; 下一文件, 忽略非文件行
          "k"     #'dired-hacks-previous-file ; 上一文件, 忽略非文件行
          "h"     #'gwp::dired-up-directory
          "l"     #'dired-view-file
          "DEL"   #'gwp::dired-up-directory   ; BACKSPACE
          "RET"   #'gwp::dired-find-alternate-file
          "K"     #'dired-kill-line           ; 移除 dired buffer 中某行, 不影响文件, 相当于过滤
          "C-S-n" #'dired-create-directory
          "C-S-f" #'dired-create-empty-file
          )

    (gwp::local-leader-def
     :keymaps 'dired-mode-map
     "k" '(gwp::dired-quit-all :which-key "kill all dired buffers")
     "c" '(dired-collapse-mode :which-key "collapse empty dirs")
     "h" '(dired-omit-mode :which-key "toggle hidden files")
     "f" '(gwp::dired-fd :which-key "fd files")
     "y" '(gwp/dired-copy-file-path :which-key "Copy file path")
     "l" '(dired-do-symlink :which-key "Make symlink")
     "o" '(dired-find-file-other-window :which-key "display in other window")
     "SPC" '(dired-view-file :which-key "preview file")
     "!" '(dired-do-async-shell-command :which-key "Async shell command")
     "S" '(gwp::dired-open-current-as-sudo :which-key "sudo open file")
     )
    (gwp::goto-leader-def
     :keymaps 'dired-mode-map
     "g" '(gwp::dired-goto-first :which-key "goto first entry")
     "e" '(gwp::dired-goto-last :which-key "goto last entry")
     )))
;; 67caa559 ends here

;; [[file:../../../gwp.note::5af5f8db][5af5f8db]]
(use-package dired-x
  :custom
  (dired-omit-verbose t)
  (dired-omit-files (rx (or
                         (seq bol (? ".") "#")
                         (seq bol "." (* anychar) eol) ; example: ".", "..", ".foo"
                         ))))
;; 5af5f8db ends here

;; [[file:../../../gwp.note::38a0a087][38a0a087]]
(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar))
;; 38a0a087 ends here
