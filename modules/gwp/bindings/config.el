;; [[file:../../../gwp.note::38249b42][38249b42]]
(gwp::goto-leader-def
  :keymaps '(prog-mode-map org-mode-map)
  ;; "g" '(beginning-of-buffer :which-key "goto first line")
  ;; "e" '(end-of-buffer :which-key "goto last line")
  ;; "l" '(end-of-line :which-key "goto the end of line")
  ;; "h" '(beginning-of-line :which-key "goto the beginning of line")
  ;; "d" '(+lookup/definition :which-key "Jump to definition")
  ;; "f" '(+lookup/file :which-key "Locate file")
  ;; "d" '(xref-find-definitions :which-key "Jump to definition")
  ;; "d" (general-simulate-key "M-.")
  "f" '(find-file-at-point :which-key "Locate file") ; emacs 自带的就很好 (ffap)
  ";" '(goto-last-change :which-key "Go to where the last edit was made")
  )
;; 38249b42 ends here

;; [[file:../../../gwp.note::2d76b8e4][2d76b8e4]]
;; meow keypad 将SPC-c SPC-x 转义为 C-c, C-x 对应的键序列
(map! :leader
      :desc "C-c" "c" #'meow-keypad-start
      :desc "C-x" "x" #'meow-keypad-start
      )

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
      :desc "Pop up scratch buffer" "X"     #'doom/open-scratch-buffer
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
       :desc "Emacs mark ring"       "m" #'counsel-mark-ring
       :desc "Jump list"             "j" #'+ivy/jump-list
       ))
;; 1c637dc8 ends here

;; [[file:../../../gwp.note::95d4be8a][95d4be8a]]
(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "Read-only mode"             "r" #'read-only-mode
       :desc "切换终端(vterm)"            "t" #'vterm-toggle
       :desc "Big mode"                   "b" #'doom-big-font-mode
       :desc "dired sidebar"              "d" #'dired-sidebar-toggle-sidebar
       :desc "Debug on error"             "D" #'toggle-debug-on-error
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
      :desc "help"
      "h" help-map)
;; e7792733 ends here

;; [[file:../../../gwp.note::19c9e88c][19c9e88c]]
;; https://stackoverflow.com/a/10395406/173271
(require 'cl-lib)
(require 'recentf)

(defun gwp::find-last-killed-file ()
  (interactive)
  (let ((active-files (cl-loop for buf  in (buffer-list)
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

;; [[file:../../../gwp.note::e44831d6][e44831d6]]
(map! :map prog-mode-map
      "C-c f" #'+format/region-or-buffer
      "C-c SPC" #'untabify              ; 将Tab 变为空格
      )
;; e44831d6 ends here

;; [[file:../../../gwp.note::5e265fdb][5e265fdb]]
(require 'init-search)
(map! :leader
      (:prefix-map ("d" . "dwim")
       :desc "select text"                           "s"   #'gwp/advanced-selection
       :desc "resize window"                         "w"   #'gwp/adjust-window-size/body
       :desc "smart parents"                         "p"   #'gwp/hydra-smartparens/body
       :desc "recent dirs"                           "r"   #'gwp::ivy-recent-dirs
       :desc "Align the current region regexp"       "a"   #'align-regexp
       :desc "Comment or uncomment lines"            "l"   #'comment-dwim
       :desc "将TAB转为空格"                         "SPC" #'untabify
       :desc "Indent region"                         "TAB" #'indent-region ; 有用
       :desc "Indent region"                         [tab] #'indent-region ; 有用
       :desc "highlight"                             "h"   gwp::symbol-overlay-map
       :desc "Send to repl"                          "s"   #'gwp/tmux-ipython-paste-region ; 和tmux配合
       :desc "Compile"                               "c"   #'compile
       :desc "统计字数"                              "g"   #'count-words
       :desc "Recompile"                             "C"   #'recompile
       :desc "Jump to definition"                    "d"   #'+lookup/definition
       :desc "Jump to references"                    "D"   #'+lookup/references
       :desc "Evaluate buffer/region"                "e"   #'+eval/buffer-or-region
       :desc "Evaluate & replace region"             "E"   #'+eval:replace-region
       :desc "Format buffer/region"                  "f"   #'+format/region-or-buffer
       :desc "Find implementations"                  "i"   #'+lookup/implementations
       :desc "Jump to documentation"                 "k"   #'+lookup/documentation
       :desc "Find type definition"                  "t"   #'+lookup/type-definition
       :desc "Delete trailing whitespace"            "w"   #'delete-trailing-whitespace
       :desc "Delete trailing newlines"              "W"   #'doom/delete-trailing-newlines
       :desc "List errors"                           "x"   #'+default/diagnostics
       ))
;; 5e265fdb ends here

;; [[file:../../../gwp.note::ac7488a5][ac7488a5]]
(map! :leader
      (:prefix-map ("g" . "git")
       :desc "Revert file"               "R"   #'vc-revert
       :desc "Copy link to remote"       "y"   #'+vc/browse-at-remote-kill
       :desc "Copy link to homepage"     "Y"   #'+vc/browse-at-remote-kill-homepage
       :desc "Manage dotfiles (yadm)"    "y"   (cmd! (magit-status "/yadm::"))
       :desc "Magit dispatch"            "/"   #'magit-dispatch
       :desc "Magit file dispatch"       "."   #'magit-file-dispatch
       :desc "Forge dispatch"            "'"   #'forge-dispatch
       :desc "Magit switch branch"       "b"   #'magit-branch-checkout
       :desc "Magit status"              "g"   #'magit-status
       :desc "Magit status here"         "G"   #'magit-status-here
       :desc "Magit file delete"         "D"   #'magit-file-delete
       :desc "Magit blame"               "B"   #'magit-blame-addition
       :desc "Magit clone"               "C"   #'magit-clone
       :desc "Magit fetch"               "F"   #'magit-fetch
       :desc "Magit buffer log"          "L"   #'magit-log-buffer-file
       :desc "Git stage file"            "S"   #'magit-stage-file
       :desc "Git unstage file"          "U"   #'magit-unstage-file
       (:prefix ("f" . "find")
        :desc "Find file"                 "f"   #'magit-find-file
        :desc "Find commit"               "c"   #'magit-show-commit
        :desc "Find issue"                "i"   #'forge-visit-issue
        :desc "Find pull request"         "p"   #'forge-visit-pullreq)
       (:prefix ("o" . "open in browser")
        :desc "Browse file or region"     "o"   #'+vc/browse-at-remote
        :desc "Browse homepage"           "h"   #'+vc/browse-at-remote-homepage
        :desc "Browse remote"             "r"   #'forge-browse-remote
        :desc "Browse commit"             "c"   #'forge-browse-commit
        :desc "Browse an issue"           "i"   #'forge-browse-issue
        :desc "Browse a pull request"     "p"   #'forge-browse-pullreq
        :desc "Browse issues"             "I"   #'forge-browse-issues
        :desc "Browse pull requests"      "P"   #'forge-browse-pullreqs)
       (:prefix ("l" . "list")
        :desc "List repositories"         "r"   #'magit-list-repositories
        :desc "List submodules"           "s"   #'magit-list-submodules
        :desc "List issues"               "i"   #'forge-list-issues
        :desc "List pull requests"        "p"   #'forge-list-pullreqs
        :desc "List notifications"        "n"   #'forge-list-notifications)
       (:prefix ("c" . "create")
        :desc "Initialize repo"           "r"   #'magit-init
        :desc "Clone repo"                "R"   #'magit-clone
        :desc "Commit"                    "c"   #'magit-commit-create
        :desc "Fixup"                     "f"   #'magit-commit-fixup
        :desc "Branch"                    "b"   #'magit-branch-and-checkout
        :desc "Issue"                     "i"   #'forge-create-issue
        :desc "Pull request"              "p"   #'forge-create-pullreq))
      )
;; ac7488a5 ends here

;; [[file:../../../gwp.note::1ad72c87][1ad72c87]]
(map! :leader
      (:prefix-map ("n" . "note/next")
       :desc "Search notes for symbol"      "*" #'+default/search-notes-for-symbol-at-point
       :desc "Org agenda"                   "a" #'org-agenda
       :desc "Find .note files"             "f" #'gwp/find-file-in-notes
       :desc "Browse notes"                 "F" #'+default/browse-notes
       :desc "Org store link"               "l" #'org-store-link
       :desc "Tags search"                  "m" #'org-tags-view
       :desc "Org capture"                  "c" #'org-capture
       :desc "org-agenda"                   "n" (cmd! (org-agenda nil "gt"))
       :desc "Goto capture"                 "N" #'org-capture-goto-target
       :desc "Todo list"                    "t" #'org-todo-list
       :desc "Search notes"                 "s" #'+default/org-notes-search
       :desc "Search org agenda headlines"  "S" #'+default/org-notes-headlines
       :desc "View search"                  "v" #'org-search-view
       ))
;; 1ad72c87 ends here

;; [[file:../../../gwp.note::6ea0d271][6ea0d271]]
(map! :leader
      (:prefix ("j" . "jump")
       :desc "org src" "o"
       #'gwp/org-babel-tangle-jump-to-org
       :desc "remote line" "l"
       #'avy-goto-line
       :desc "remote position (char-2)" "f"
       #'avy-goto-char-2
       :desc "emacs mark ring" "m"
       #'gwp::hydra-mark-ring-pop/body
       ))
;; 6ea0d271 ends here

;; [[file:../../../gwp.note::d2dc925d][d2dc925d]]
(require 'expand-region)
(map! :leader
      (:prefix-map ("v" . "visual")
       :desc "expand region"            "v" #'er/expand-region
       :desc "select text"              "s" #'gwp/advanced-selection
       :desc "jump to emacs mark ring"  "j" #'gwp::hydra-mark-ring-pop/body
       :desc "multi-cursor mode"        "m" #'gwp::hydra-multiedit/body))
;; d2dc925d ends here

;; [[file:../../../gwp.note::bc190292][bc190292]]
(map! :leader
      (:prefix-map ("e" . "edit")
       :desc "copy remote line" "l" #'avy-copy-line
       :desc "copy remote lines" "L" #'avy-copy-region
       ))
;; bc190292 ends here

;; [[file:../../../gwp.note::bc190292][bc190292]]
(map! :leader
      (:prefix-map ("y" . "yank")
       :desc "goldendict word"               "g"   #'gwp::goldendict-from-clipboard
       :desc "Snippet"                       "s"   #'yas-insert-snippet
       :desc "From clipboard"                "y"   #'gwp::yank-dwim
       :desc "Current file name"             "f"   #'+default/insert-file-path
       :desc "Current file path"             "F"   (cmd!! #'+default/insert-file-path t)
       :desc "Unicode"                       "u"   #'insert-char
       ))
;; bc190292 ends here
