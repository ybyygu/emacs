;; [[file:../doom.note::edd7000d][edd7000d]]
(use-package dired
  :custom
  ;; 方便多个文件夹文件互动
  ;; Set this variable to non-nil, Dired will try to guess a default
  ;; target directory. This means: if there is a dired buffer
  ;; displayed in the next window, use its current subdir, instead
  ;; of the current subdir of this dired buffer. The target is used
  ;; in the prompt for file copy, rename etc.
  (dired-dwim-target t)

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
  (progn
    ;; 安全第一
    (setq delete-by-moving-to-trash t)

    ;; 用于在dired中复制当前文件的全路径.
    (defun gwp/dired-copy-file-path()
      (interactive)
      (let ((current-prefix-arg '(0)))
        (call-interactively 'dired-copy-filename-as-kill)))

    (map! :map dired-mode-map
          :localleader
          :desc "Copy file path"
          :n "y" #'gwp/dired-copy-file-path
          :desc "Make symlink"
          :n "l" #'dired-do-symlink
          :desc "Async shell command"
          :n "!" #'dired-do-async-shell-command
          )

    ;; 使用BACKSPACE来上一级目录, 使用Ctrl-shift-n来新建目录(默认为"+")
    (map! :map dired-mode-map
          :nv "DEL"   #'dired-up-directory       ; BACKSPACE
          :nv "C-S-n" #'dired-create-directory)))

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
;; edd7000d ends here

;; [[file:../doom.note::67102cd3][67102cd3]]
(use-package dired-x
  :config
  (progn
    (setq dired-omit-verbose t)
    ;; (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))))

(provide 'init-dired)
;; 67102cd3 ends here
