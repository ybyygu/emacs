;; [[file:../../../gwp.note::2d76b8e4][2d76b8e4]]
;; https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(require 'init-proxy)

(defun gwp::switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(map! :leader
      :desc "Save buffer" "SPC"             #'gwp::mark-and-save-buffer
      :desc "Universal argument"    "u"     #'universal-argument
      :desc "Pop up scratch buffer" "x"     #'doom/open-scratch-buffer
      :desc "Jump to previous mark" ","     #'gwp::jump-to-previous-mark
      :desc "Switch to previous buffer" "`" #'gwp::switch-to-previous-buffer
      )

(map!
 "C-`"                             #'+popup/toggle
 "C-~"                             #'+popup/raise
 "C-x p"                           #'+popup/other)
;; 2d76b8e4 ends here

;; [[file:../../../gwp.note::46f457b2][46f457b2]]
(map! :leader
      :desc "window/frame" "w" gwp::window-map
      )
;; 46f457b2 ends here

;; [[file:../../../gwp.note::493c2a26][493c2a26]]
(use-package recentf
  :custom
  ;; then run M-x recentf-cleanup to make it work.
  (recentf-exclude '("/tmp/"
                     "/ssh:"
                     "/sudo:"
                     "/scp:"
                     "/scpx:"
                     "/ssh:"
                     ;; "\\.pdf$"
                     "\\.png$"
                     "autosave$"
                     ;; "\\.odt$"
                     "\\.note_archive$"
                     "_workspaces"
                     ".*/COMMIT_EDITMSG$" ; magit 临时编辑文件
                     ;; ".*/$"               ; 剔除目录
                     ))
  (recentf-max-saved-items 9999)   ; the default is only 20
  (recentf-keep '(gwp::recentf-keep-p))
  ;; clean up items when has been idle 1 hour
  ;; (recentf-auto-cleanup 3600)
  (recentf-auto-cleanup 'never)         ; doom 在退出时清理
  )

(defun gwp::recentf-keep-p (file)
  "仅保留本地可读文件"
  (not (file-remote-p file))
  ;; (and (not (file-remote-p file))
  ;;      (not (file-directory-p file)))
  )
;; 493c2a26 ends here

;; [[file:../../../gwp.note::e13c7903][e13c7903]]
(defun spacemacs/open-in-external-app (file-path)
  "Open `file-path' in external application."
  (let ((process-connection-type nil))
    (start-process "" nil "xdg-open" file-path)))

(defun spacemacs/open-file-or-directory-in-external-app (arg)
  "Open current file in external application.
If the universal prefix argument is used then open the folder
containing the current file by the default explorer.
If two universal prefix arguments are used, then prompt for command to use."
  (interactive "P")
  (if (equal arg '(4))                  ; C-u
      (spacemacs/open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (if (equal arg '(16))         ; C-u C-u
              (progn
                (let ((program (read-shell-command "Open current file with: ")))
                  (call-process program nil 0 nil file-path)))
            (spacemacs/open-in-external-app file-path))
        (message "No file associated to this buffer.")))))
;; e13c7903 ends here

;; [[file:../../../gwp.note::d7dce976][d7dce976]]
(map! :leader
      ;; 注意 "file/find"特意与+evil-bindings.el中定义的不同, 用于覆盖原按键
      (:prefix-map ("f" . "file/find")
       :desc "Open bookmarks"              "b"   #'counsel-bookmark ; 有用
       :desc "Recent files"                "r"   #'recentf-open-files
       :desc "Jump to Dired buffer"        "j"   #'dired-jump ; 有用
       :desc "Find .note files"            "n"   #'gwp/find-file-in-notes
       :desc "Delete this file"            "D"   #'doom/delete-this-file
       :desc "Find file in emacs.d"        "e"   #'doom/find-file-in-emacsd
       :desc "Browse emacs.d"              "E"   #'doom/browse-in-emacsd
       :desc "Find file"                   "f"   #'find-file
       :desc "Locate file"                 "l"   #'locate
       :desc "Open file/dir externally"    "o"   #'spacemacs/open-file-or-directory-in-external-app
       :desc "Find file in private config" "p"   #'doom/find-file-in-private-config
       :desc "Browse private config"       "P"   #'doom/open-private-config
       :desc "Copy this file"              "C"   #'doom/copy-this-file
       :desc "Rename/move file"            "R"   #'doom/move-this-file
       :desc "Save file"                   "s"   #'save-buffer
       :desc "Save file as..."             "S"   #'write-file
       :desc "Sudo find file"              "u"   #'doom/sudo-find-file
       :desc "Sudo this file"              "U"   #'doom/sudo-this-file
      ))
;; d7dce976 ends here

;; [[file:../../../gwp.note::321efaf0][321efaf0]]
(map! :leader
      (:prefix-map ("s" . "search")
       :desc "Search buffer"                "s" #'swiper ; 一行仅一个匹配?
       :desc "Search buffer at point"       "S" #'swiper-isearch-thing-at-point
       :desc "Search all notes"             "n" #'gwp/search-all-notes ; 全局搜索.note文件
       :desc "Search buffer from clipboard" "y" #'gwp::swiper-from-clipboard
       :desc "Search all open buffers"      "B" #'swiper-all
       :desc "Jump to search occurrence"    ";" #'gwp/evil-ex-search-avy-jump
       :desc "搜索当前文件夹文件名"         "f" #'find-file-in-current-directory-by-selected ; 搜索文件名
       :desc "Jump to symbol"               "i" #'imenu
       :desc "Locate file"                  "l" #'counsel-locate
       :desc "Jump to bookmark"             "m" #'bookmark-jump
       (:prefix-map ("h" . "highlight")
        :desc "highlight symbol at point"         "." #'highlight-symbol-at-point
        :desc "highlight REGEXP"                  "h" #'highlight-regexp
        :desc "unhighlight regexp"                "u" #'unhighlight-regexp)))
;; 321efaf0 ends here

;; [[file:../../../gwp.note::1c637dc8][1c637dc8]]
(map! :leader
      (:prefix-map ("r" . "resume/rings")
       :desc "Open bookmarks"        "b" #'counsel-bookmark
       :desc "Resume last search"    "l" #'ivy-resume
       :desc "Last change"           "c" #'gwp::hydra-last-change/body
       ))
;; 1c637dc8 ends here

;; [[file:../../../gwp.note::95d4be8a][95d4be8a]]
(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "Read-only mode"             "r" #'read-only-mode
       :desc "切换终端(vterm)"            "t" #'vterm-toggle
       :desc "Big mode"                   "b" #'doom-big-font-mode
       :desc "Debug on error"             "d" #'toggle-debug-on-error
       :desc "Frame fullscreen"           "F" #'toggle-frame-fullscreen
       :desc "Indent style"               "I" #'doom/toggle-indent-style
       :desc "Line numbers"               "l" #'gwp::toggle-line-numbers
       :desc "Spell checker"              "s" #'spell-fu-mode
       :desc "Socks proxy"                "p" #'proxy-socks-toggle
       :desc "Soft line wrapping"         "w" #'visual-line-mode
       ))
;; 95d4be8a ends here

;; [[file:../../../gwp.note::e7792733][e7792733]]
(map! :leader
      :desc "help"                  "h"    help-map
      )
;; e7792733 ends here

;; [[file:../../../gwp.note::19c9e88c][19c9e88c]]
;; https://stackoverflow.com/a/10395406/173271
(require 'cl-lib)
(require 'recentf)

(defun gwp::find-last-killed-file ()
  (interactive)
  (let ((active-files (cl-loop for buf in (buffer-list)
                            when (buffer-file-name buf) collect it)))
    (cl-loop for file in recentf-list
          unless (member file active-files) return (find-file file))))

(map! :leader
      (:prefix-map ("b" . "buffer/bookmark")
       :desc "Switch workspace buffer"     "b"   #'persp-switch-to-buffer
       :desc "Switch buffer"               "B"   #'switch-to-buffer
       :desc "Kill buffer"                 "d"   #'kill-current-buffer
       :desc "delete buffer file"          "D"   #'doom/delete-this-file
       :desc "Kill buffer"                 "k"   #'kill-current-buffer
       :desc "Kill all buffers"            "K"   #'doom/kill-all-buffers
       :desc "Revert buffer"               "r"   #'revert-buffer
       :desc "Previous buffer"             "p"   #'previous-buffer
       :desc "Next buffer"                 "n"   #'next-buffer
       :desc "Save buffer"                 "s"   #'basic-save-buffer
       :desc "Save file as..."             "S"   #'write-file
       :desc "Clone indirect buffer"       "c"   #'clone-indirect-buffer-other-window
       :desc "Toggle narrowing"            "-"   #'doom/toggle-narrow-buffer
       :desc "Switch to last buffer"       "l"   #'evil-switch-to-windows-last-buffer
       :desc "New empty buffer"            "N"   #'evil-buffer-new
       :desc "ibuffer"                     "i"   #'ibuffer
       :desc "Open in new frame"           "o"   #'gwp::display-current-buffer-other-frame
       :desc "Kill other buffers"          "O"   #'doom/kill-other-buffers
       :desc "reopen killed file"          "u"   #'gwp::find-last-killed-file
       :desc "copy file path"              "y"   #'+default/yank-buffer-path
       :desc "Bury buffer"                 "z"   #'bury-buffer
       :desc "Kill buried buffers"         "Z"   #'doom/kill-buried-buffers
       :desc "Set bookmark"                "m"   #'bookmark-set
       ))
;; 19c9e88c ends here

;; [[file:../../../gwp.note::9a5a2bc3][9a5a2bc3]]
;;; <leader> q --- quit/session
(map! :leader
      (:prefix-map ("q" . "quit/session")
       :desc "Quit Emacs"                   "q" #'save-buffers-kill-terminal
       :desc "Reloads your private config"  "r" #'doom/reload
       :desc "Delete frame"                 "f" #'delete-frame
       :desc "Clear current frame"          "F" #'doom/kill-all-buffers
       :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
       :desc "Quit Emacs without saving"    "Q" #'evil-quit-all-with-error-code
       :desc "Quick save current session"   "s" #'doom/quicksave-session
       :desc "Restore last session"         "l" #'doom/quickload-session
       :desc "Save session to file"         "S" #'doom/save-session
       :desc "Restore session from file"    "L" #'doom/load-session
       :desc "Restart Emacs"                "R" #'doom/restart
       ))
;; 9a5a2bc3 ends here

;; [[file:../../../gwp.note::6b10b827][6b10b827]]
(after! persp-mode
  (setq persp-auto-save-opt 0))

;;;###autoload
(defun gwp::workspace/load-or-switch (name)
  "Load or switch to a workspace."
  (interactive
   (list
    (completing-read
     "Workspace to load: "
     (persp-list-persp-names-in-file
      (expand-file-name +workspaces-data-file persp-save-dir)))))
  (if (+workspace-exists-p name)
      (+workspace/switch-to name)
    (if (not (+workspace-load name))
        (+workspace-error (format "Couldn't load workspace %s" name))
      (+workspace/switch-to name)))
  (+workspace/display))

;;;###autoload
(defun gwp::workspace/new-named (name)
  "Create a new workspace with a given NAME."
  (interactive "sWorkspace Name: ")
  (+workspace/new name)
  (+workspace/switch-to name))
;; 6b10b827 ends here

;; [[file:../../../gwp.note::2598d642][2598d642]]
(map! :leader
      (:prefix-map ("l" . "load/workspace") ;; 用tab不方便
       :desc "Load or witch workspace"   "l"   #'gwp::workspace/load-or-switch
       :desc "Switch workspace"          "TAB" #'+workspace/switch-to ; 调整下
       :desc "Switch to last workspace"  "b"   #'+workspace/other ; 换个好按的
       :desc "Next workspace"            "n"   #'+workspace/switch-right
       :desc "Previous workspace"        "p"   #'+workspace/switch-left
       :desc "Display tab bar"           "."   #'+workspace/display
       :desc "Switch to last workspace"  "`"   #'+workspace/other
       :desc "New named workspace"       "N"   #'gwp::workspace/new-named
       :desc "Save workspace to file"    "s"   #'+workspace/save
       :desc "Delete session"            "x"   #'+workspace/kill-session
       :desc "Delete this workspace"     "d"   #'+workspace/delete
       :desc "Rename workspace"          "r"   #'+workspace/rename
       :desc "Restore last session"      "R"   #'+workspace/restore-last-session
       :desc "Switch to 1st workspace"   "1"   #'+workspace/switch-to-0
       :desc "Switch to 2nd workspace"   "2"   #'+workspace/switch-to-1
       :desc "Switch to 3rd workspace"   "3"   #'+workspace/switch-to-2
       :desc "Switch to 4th workspace"   "4"   #'+workspace/switch-to-3
       :desc "Switch to 5th workspace"   "5"   #'+workspace/switch-to-4
       :desc "Switch to 6th workspace"   "6"   #'+workspace/switch-to-5
       :desc "Switch to 7th workspace"   "7"   #'+workspace/switch-to-6
       :desc "Switch to 8th workspace"   "8"   #'+workspace/switch-to-7
       :desc "Switch to 9th workspace"   "9"   #'+workspace/switch-to-8
       :desc "Switch to final workspace" "0"   #'+workspace/switch-to-final
       ))
;; 2598d642 ends here

;; [[file:../../../gwp.note::12a811d1][12a811d1]]
(defun gwp/open-in-gnome-terminal (the-directory)
  "Open `the-directory' in external gnome-terminal."
  (let ((process-connection-type nil))
    ;; (start-process "" nil "terminal-dwim.sh" (concat "--working-directory=" the-directory) "-e" "tmux")
    (start-process "" nil "alacritty" (concat "--working-directory=" the-directory) "-e" "tmux")
    ))

(defun gwp/open-terminal-here ()
  "Open the current dir in a new terminal window"
  (interactive)
  (let ((default-directory (or (and (eq major-mode 'dired-mode)
                                    (dired-current-directory))
                               default-directory)))
    (gwp/open-in-gnome-terminal (expand-file-name default-directory))))
;; 12a811d1 ends here

;; [[file:../../../gwp.note::fc304196][fc304196]]
(map! :leader
      (:prefix-map ("o" . "org/open")
       :desc "Org attachment"        "a" #'org-attach
       :desc "org-agenda (GTD)"      "n" (cmd! (org-agenda nil "gt"))
       :desc "org-capture"           "c" #'org-capture
       :desc "Tags search"           "m" #'org-tags-view
       :desc "View search"           "v" #'org-search-view
       :desc "New frame"             "f" #'make-frame
       :desc "Select frame"          "F" #'select-frame-by-name
       :desc "REPL"                  "r" #'+eval/open-repl-other-window
       :desc "REPL (same window)"    "R" #'+eval/open-repl-same-window
       :desc "Toggle vterm popup"    "t" #'+vterm/toggle
       :desc "Open termerinal here"  "T" #'gwp/open-terminal-here ; 打开外部terminal
       ))
;; fc304196 ends here
