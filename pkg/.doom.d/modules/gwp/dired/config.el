;; [[file:../../../../../doom.note::303a749e][303a749e]]
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
;; 303a749e ends here

;; [[file:../../../../../doom.note::23a898ff][23a898ff]]
(defun gwp::dired-remove-empty-dirs ()
  (interactive)
  (when (equal major-mode 'dired-mode)
    (dired-do-shell-command "rmdir -p")))
;; 23a898ff ends here

;; [[file:../../../../../doom.note::edd7000d][edd7000d]]
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
  ;;  -F : Classify filenames by appending '*' to executables,
  ;;       '/' to directories, etc.
  (dired-listing-switches "-alGhvF --group-directories-first") ; default: "-al"

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

    (map! :map dired-mode-map
          :localleader
          :desc "kill all dired buffers" "k" #'gwp::dired-quit-all
          :desc "collapse empty dirs" "c" #'dired-collapse-mode
          :desc "toggle hidden files" "h" #'dired-omit-mode
          :desc "fd files" "f" #'gwp::dired-fd
          :desc "Copy file path" "y" #'gwp/dired-copy-file-path
          :desc "Make symlink" "l" #'dired-do-symlink
          :desc "display in other window" "o" #'dired-find-file-other-window
          :desc "preview file" "SPC" #'dired-view-file
          :desc "sudo open file" "S" #'gwp::dired-open-current-as-sudo
          :desc "Async shell command" "!" #'dired-do-async-shell-command
          )

    ;; 很有用
    (gwp-leader-def
      :keymaps 'dired-mode-map
      "SPC" #'dired-view-file)

    ;; 使用BACKSPACE来上一级目录, 使用Ctrl-shift-n来新建目录(默认为"+")
    (map! :map dired-mode-map
          :nv "q" #'gwp::dired-quit-window
          :nv "j" #'dired-hacks-next-file       ; 下一文件, 忽略非文件行
          :nv "k" #'dired-hacks-previous-file   ; 上一文件, 忽略非文件行
          :nv "DEL"   #'gwp::dired-up-directory ; BACKSPACE
          :nv "RET"   #'gwp::dired-find-alternate-file
          :nv "C-S-n" #'dired-create-directory
          :nv "C-S-f" #'dired-create-empty-file
          :n "gh" #'gwp::dired-goto-first
          :n "gg" #'gwp::dired-goto-first
          :n "G" #'gwp::dired-goto-last
          )))

;; dired 默认用 emacs 模式
;; (use-package evil
;;   :config
;;   (evil-set-initial-state 'dired-mode 'emacs))
;; edd7000d ends here

;; [[file:../../../../../doom.note::67102cd3][67102cd3]]
(use-package dired-x
  :config
  (progn
    (setq dired-omit-verbose t)
    ;; (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))))
;; 67102cd3 ends here

;; [[file:../../../../../doom.note::d356ffd8][d356ffd8]]
;; 可生成的文件过滤视图
(use-package dired-filter
  :config
  (map! :map dired-mode-map
        :localleader
        (:prefix-map ("/" . "dired-filter")
         :desc "by file" "f" #'dired-filter-by-file
         :desc "by name" "n" #'dired-filter-by-name
         :desc "by extension" "e" #'dired-filter-by-extension
         :desc "by regexp" "r" #'dired-filter-by-regexp
         :desc "by directory" "d" #'dired-filter-by-directory
         :desc "reset" "/" #'dired-filter-pop-all
         )))
;; d356ffd8 ends here

;; [[file:../../../../../doom.note::102cb018][102cb018]]
(use-package dired-collapse
  :hook (dired-mode-hook . dired-collapse-mode))
;; 102cb018 ends here

;; [[file:../../../../../doom.note::d7711b52][d7711b52]]
(defun gwp::dired-open-current-as-sudo ()
  "open current file as sudo"
  (interactive)
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name (dired-file-name-at-point)))))
    (find-file tramp-file-name)))
;; d7711b52 ends here
