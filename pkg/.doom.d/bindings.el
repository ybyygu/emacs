;; [[file:../../doom.note::2d76b8e4][2d76b8e4]]
(defun gwp::mark-and-save-buffer()
  "标记光标所在位置, 并保存buffer"
  (interactive)
  (call-interactively #'gwp::mark-current-position)
  (save-buffer))

(map! :leader
      :desc "Save buffer" "SPC"         #'gwp::mark-and-save-buffer
      :desc "Universal argument"    "u" #'universal-argument
      :desc "Pop up scratch buffer" "x" #'doom/open-scratch-buffer
      :desc "Jump to previous mark" "," #'gwp::jump-to-previous-mark
      :desc "Switch to last buffer" "`" #'evil-switch-to-windows-last-buffer)

(map!
 "C-`"                             #'+popup/toggle
 "C-~"                             #'+popup/raise
 "C-x p"                           #'+popup/other)
;; 2d76b8e4 ends here

;; [[file:../../doom.note::19c9e88c][19c9e88c]]
;; https://stackoverflow.com/a/10395406/173271
(require 'cl)
(require 'recentf)

(defun gwp::find-last-killed-file ()
  (interactive)
  (let ((active-files (loop for buf in (buffer-list)
                            when (buffer-file-name buf) collect it)))
    (loop for file in recentf-list
          unless (member file active-files) return (find-file file))))

(map! :leader
      (:prefix-map ("b" . "buffer/bookmark")
       :desc "Switch workspace buffer"     "b"   #'persp-switch-to-buffer
       :desc "Switch buffer"               "B"   #'switch-to-buffer
       :desc "Kill buffer"                 "d"   #'kill-current-buffer
       :desc "Kill buffer"                 "k"   #'kill-current-buffer
       :desc "Kill all buffers"            "K"   #'doom/kill-all-buffers
       :desc "Revert buffer"               "r"   #'revert-buffer
       :desc "Previous buffer"             "p"   #'previous-buffer
       :desc "Next buffer"                 "n"   #'next-buffer
       :desc "Save buffer"                 "s"   #'basic-save-buffer
       :desc "Clone indirect buffer"       "c"   #'clone-indirect-buffer
       :desc "Toggle narrowing"            "-"   #'doom/toggle-narrow-buffer
       :desc "Switch to last buffer"       "l"   #'evil-switch-to-windows-last-buffer
       :desc "New empty buffer"            "N"   #'evil-buffer-new
       :desc "ibuffer"                     "i"   #'ibuffer
       :desc "Open in new frame"           "o"   #'gwp/display-current-buffer-other-frame
       :desc "Kill other buffers"          "O"   #'doom/kill-other-buffers
       :desc "Save all buffers"            "S"   #'evil-write-all
       :desc "reopen killed file"          "u"   #'gwp::find-last-killed-file
       :desc "copy file path"              "y"   #'+default/yank-buffer-path
       :desc "Bury buffer"                 "z"   #'bury-buffer
       :desc "Kill buried buffers"         "Z"   #'doom/kill-buried-buffers
       :desc "Set bookmark"                "m"   #'bookmark-set
       :desc "Delete bookmark"             "M"   #'bookmark-delete
       ))
;; 19c9e88c ends here

;; [[file:../../doom.note::e78a01dd][e78a01dd]]
(map! :leader
      (:prefix-map ("c" . "code/compile")
       :desc "Align the current region regexp"       "a"   #'align-regexp
       :desc "Comment or uncomment lines"            "l"   #'gwp/comment-or-uncomment-dwim
       :desc "Copy & comment"                        "y"   #'evilnc-copy-and-comment-lines
       :desc "indent region"                         "TAB" #'indent-region ; 有用
       :desc "indent region"                         [tab] #'indent-region ; 有用
       :desc "Compile"                               "c"   #'compile
       :desc "Recompile"                             "C"   #'recompile
       :desc "highlight"                             "h"   gwp::symbol-overlay-map
       :desc "Jump to definition"                    "d"   #'+lookup/definition
       :desc "Jump to references"                    "D"   #'+lookup/references
       :desc "Evaluate buffer/region"                "e"   #'+eval/buffer-or-region
       :desc "Evaluate & replace region"             "E"   #'+eval:replace-region
       :desc "Format buffer/region"                  "f"   #'+format/region-or-buffer
       :desc "Find implementations"                  "i"   #'+lookup/implementations
       :desc "Jump to documentation"                 "k"   #'+lookup/documentation
       :desc "Send to repl"                          "s"   #'gwp/tmux-ipython-paste-region ; 和tmux配合
       :desc "Find type definition"                  "t"   #'+lookup/type-definition
       :desc "Delete trailing whitespace"            "w"   #'delete-trailing-whitespace
       :desc "Delete trailing newlines"              "W"   #'doom/delete-trailing-newlines
       :desc "List errors"                           "x"   #'+default/diagnostics
       ))
;; e78a01dd ends here

;; [[file:../../doom.note::5e265fdb][5e265fdb]]
(map! :leader
      (:prefix-map ("d" . "dodo")
       :desc "select text"   "s" #'gwp/advanced-selection
       :desc "resize window" "w" #'gwp/adjust-window-size/body
       :desc "smart parents" "p" #'gwp/hydra-smartparens/body
       ))
;; 5e265fdb ends here

;; [[file:../../doom.note::6c75cff0][6c75cff0]]
(map! :leader
      (:prefix-map ("e" . "external")
       :desc "程序窗口管理" "w" #'counsel-wmctrl
       :desc "启动桌面程序" "a" #'counsel-linux-app
       ))
;; 6c75cff0 ends here

;; [[file:../../doom.note::9ef458a0][9ef458a0]]
(map! :leader
      ;; 注意 "file/find"特意与+evil-bindings.el中定义的不同, 用于覆盖原按键
      (:prefix-map ("f" . "file/find")
       :desc "Open bookmarks"              "b"   #'counsel-bookmark ; 有用
       :desc "Jump to Dired buffer"        "j"   #'dired-jump ; 有用
       :desc "Find .note files"            "n"   #'gwp/find-file-in-notes
       :desc "Find directory"              "d"   #'+default/dired
       :desc "Delete this file"            "D"   #'doom/delete-this-file
       :desc "Find file in emacs.d"        "e"   #'doom/find-file-in-emacsd
       :desc "Browse emacs.d"              "E"   #'doom/browse-in-emacsd
       :desc "Find file"                   "f"   #'find-file
       :desc "Find file from here"         "F"   #'+default/find-file-under-here
       :desc "Locate file"                 "l"   #'locate
       :desc "Open file/dir externally"    "o"   #'spacemacs/open-file-or-directory-in-external-app
       :desc "Find file in private config" "p"   #'doom/find-file-in-private-config
       :desc "Browse private config"       "P"   #'doom/open-private-config
       :desc "Recent files"                "r"   #'recentf-open-files
       :desc "Copy this file"              "C"   #'doom/copy-this-file
       :desc "Rename/move file"            "R"   #'doom/move-this-file
       :desc "Save file"                   "s"   #'save-buffer
       :desc "Save file as..."             "S"   #'write-file
       :desc "Sudo find file"              "u"   #'doom/sudo-find-file
       :desc "Sudo this file"              "U"   #'doom/sudo-this-file
       :desc "Yank file path"              "y"   #'+default/yank-buffer-path
       :desc "Yank file path from project" "Y"   #'+default/yank-buffer-path-relative-to-project)
      )
;; 9ef458a0 ends here

;; [[file:../../doom.note::ac7488a5][ac7488a5]]
(map! :leader
      (:prefix-map ("g" . "git")
       :desc "Revert file"                 "R"   #'vc-revert
       :desc "Copy link to remote"         "y"   #'+vc/browse-at-remote-kill
       :desc "Copy link to homepage"       "Y"   #'+vc/browse-at-remote-kill-homepage
       (:when (featurep! :ui hydra)
        :desc "SMerge"                    "m"   #'+vc/smerge-hydra/body)
       (:when (featurep! :ui vc-gutter)
        (:when (featurep! :ui hydra)
         :desc "VCGutter"                "."   #'+vc/gutter-hydra/body)
        :desc "Revert hunk"               "r"   #'git-gutter:revert-hunk
        :desc "Git stage hunk"            "s"   #'git-gutter:stage-hunk
        :desc "Git time machine"          "t"   #'git-timemachine-toggle
        :desc "Jump to next hunk"         "]"   #'git-gutter:next-hunk
        :desc "Jump to previous hunk"     "["   #'git-gutter:previous-hunk)
       (:when (featurep! :gwp magit) ; 改用自定义版
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
         :desc "Find gitconfig file"       "g"   #'magit-find-git-config-file
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
         (:when (featurep! :tools gist)
          :desc "List gists"              "g"   #'+gist:list)
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
         :desc "Pull request"              "p"   #'forge-create-pullreq)))
      )
;; ac7488a5 ends here

;; [[file:../../doom.note::e7792733][e7792733]]
(map! :leader
      :desc "help"                  "h"    help-map
      )
;; e7792733 ends here

;; [[file:../../doom.note::4a4573c2][4a4573c2]]
(map! :leader
      (:prefix-map ("i" . "insert")
       :desc "Snippet"                       "s"   #'yas-insert-snippet
       :desc "From clipboard"                "y"   #'+default/yank-pop
       :desc "Current file name"             "f"   #'+default/insert-file-path
       :desc "Current file path"             "F"   (cmd!! #'+default/insert-file-path t)
       :desc "Evil ex path"                  "p"   (cmd! (evil-ex "R!echo "))
       :desc "From evil register"            "r"   #'evil-ex-registers
       :desc "Unicode"                       "u"   #'insert-char
       ))
;; 4a4573c2 ends here

;; [[file:../../doom.note::6ea0d271][6ea0d271]]
(map! :leader
      (:prefix ("j" . "jump")
       :desc "org src"                "o" #'gwp/org-babel-tangle-jump-to-org
       :desc "search occurrence"      "j" #'gwp/evil-ex-search-avy-jump
       :desc "avy line"               "l" #'evil-avy-goto-line
       :desc "emacs mark ring"        "m" #'gwp::hydra-mark-ring-pop/body
       ))
;; 6ea0d271 ends here

;; [[file:../../doom.note::2598d642][2598d642]]
(map! :leader
      (:prefix-map ("l" . "load/workspace") ;; 用tab不方便
       :desc "Load or witch workspace"   "l"   #'gwp/workspace/load-or-switch
       :desc "Switch workspace"          "TAB" #'+workspace/switch-to ; 调整下
       :desc "Switch to last workspace"  "b"   #'+workspace/other ; 换个好按的
       :desc "Next workspace"            "n"   #'+workspace/switch-right
       :desc "Previous workspace"        "p"   #'+workspace/switch-left
       :desc "Display tab bar"           "."   #'+workspace/display
       :desc "Switch to last workspace"  "`"   #'+workspace/other
       :desc "New named workspace"       "N"   #'+workspace/new-named
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

;; [[file:../../doom.note::1ad72c87][1ad72c87]]
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

;; [[file:../../doom.note::fc304196][fc304196]]
(map! :leader
      (:prefix-map ("o" . "org/open")
       :desc "Org attachment"        "a" #'org-attach
       :desc "Todo list"             "t" #'org-todo-list
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

;; [[file:../../doom.note::1f0c2364][1f0c2364]]
(map! :leader
      (:prefix-map ("q" . "quit/session")
       :desc "Quit Emacs"                   "q" #'save-buffers-kill-terminal
       :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
       :desc "Restart Emacs"                "R" #'doom/restart
       :desc "Delete frame"                 "f" #'delete-frame
       :desc "Clear current frame"          "F" #'doom/kill-all-buffers
       :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
       :desc "Quit Emacs without saving"    "Q" #'evil-quit-all-with-error-code
       :desc "Quick save current session"   "s" #'doom/quicksave-session
       :desc "Restore last session"         "l" #'doom/quickload-session
       :desc "Save session to file"         "S" #'doom/save-session
       :desc "Restore session from file"    "L" #'doom/load-session
       ))
;; 1f0c2364 ends here

;; [[file:../../doom.note::1c637dc8][1c637dc8]]
(map! :leader
      (:prefix-map ("r" . "resume/rings")
       :desc "Open bookmarks"        "b" #'counsel-bookmark
       :desc "Resume last search"    "l" #'ivy-resume
       :desc "Evil registers"        "e" #'counsel-register
       :desc "Evil mark ring"        "r" #'counsel-mark-ring
       :desc "Evil markers"          "m" #'counsel-evil-marks
       :desc "Restore selection"     "v" #'evil-visual-restore
       :desc "Restore insert point"  "i" #'evil-insert-resume
       :desc "Last change"           "c" #'gwp::hydra-last-change/body
       :desc "Jump list"             "j" #'+ivy/jump-list
       ))
;; 1c637dc8 ends here

;; [[file:../../doom.note::321efaf0][321efaf0]]
(map! :leader
      :desc "Search buffer at point"        "*" #'swiper-isearch-thing-at-point
      (:prefix-map ("s" . "search")
       :desc "Search all notes"             "n" #'gwp/search-all-notes ; 全局搜索.note文件
       :desc "Search buffer"                "s" #'+default/search-buffer ; 走isearch路线, 可一行多个匹配
       :desc "Search buffer at point"       "S" #'swiper-isearch-thing-at-point
       :desc "Search buffer"                "b" #'swiper ; 一行仅一个匹配?
       :desc "Search buffer from clipboard" "y" #'gwp::swiper-from-clipboard
       :desc "Search all open buffers"      "B" #'swiper-all
       :desc "Jump to search occurrence"    ";" #'gwp/evil-ex-search-avy-jump
       :desc "Search current directory"     "d" #'+default/search-cwd
       :desc "Search other directory"       "D" #'+default/search-other-cwd
       :desc "Search .emacs.d"              "e" #'+default/search-emacsd
       :desc "fd in current dir"            "f" #'find-file-in-project-by-selected ; 搜索文件名
       :desc "Jump to symbol"               "i" #'imenu
       :desc "Locate file"                  "l" #'counsel-locate
       :desc "Jump list"                    "j" #'evil-show-jumps
       :desc "Jump to bookmark"             "m" #'bookmark-jump
       :desc "Look up online"               "o" #'+lookup/online
       :desc "Look up online (select)"      "O" #'+lookup/online-select
       :desc "Search project"               "p" #'+default/search-project
       :desc "Search other project"         "P" #'+default/search-other-project
       :desc "Jump to mark"                 "r" #'evil-show-marks
       (:prefix-map ("h" . "highlight")
        :desc "highlight symbol at point"         "." #'highlight-symbol-at-point
        :desc "highlight REGEXP"                  "h" #'highlight-regexp
        :desc "unhighlight regexp"                "u" #'unhighlight-regexp)))
;; 321efaf0 ends here

;; [[file:../../doom.note::95d4be8a][95d4be8a]]
(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "切换终端(vterm)"            "t" #'vterm-toggle
       :desc "Big mode"                   "b" #'doom-big-font-mode
       :desc "Debug on error"             "d" #'toggle-debug-on-error
       :desc "Flycheck"                   "f" #'flycheck-mode
       :desc "Frame fullscreen"           "F" #'toggle-frame-fullscreen
       :desc "Indent style"               "I" #'doom/toggle-indent-style
       :desc "Line numbers"               "l" #'gwp::toggle-line-numbers
       :desc "Read-only mode"             "r" #'read-only-mode
       :desc "Spell checker"              "s" #'spell-fu-mode
       ;; :desc "Spell checker"              "s" #'flyspell-mode
       :desc "Socks proxy"                "p" #'proxy-socks-toggle
       :desc "Soft line wrapping"         "w" #'visual-line-mode
       :desc "Zen mode"                   "z" #'+zen/toggle
       :desc "Zen mode (fullscreen)"      "Z" #'+zen/toggle-fullscreen
       ))
;; 95d4be8a ends here

;; [[file:../../doom.note::d2dc925d][d2dc925d]]
(map! :leader
      (:prefix-map ("v" . "visual")
       :desc "expand region"            "v" #'er/expand-region
       :desc "select text"              "s" #'gwp/advanced-selection
       :desc "jump to emacs mark ring"  "j" #'gwp::hydra-mark-ring-pop/body
       :desc "multi-cursor mode"        "m" #'gwp::hydra-multiedit/body))
;; d2dc925d ends here

;; [[file:../../doom.note::011dce65][011dce65]]
(map! :leader
      :desc "window"                "w"    evil-window-map ; 默认为C-w
      :desc "frame"                 "W"    ctl-x-5-map     ; 换个容易按的键位
      )
;; 011dce65 ends here

;; [[file:../../doom.note::bc190292][bc190292]]
(map! :leader
      (:prefix-map ("y" . "yank")
       :desc "Snippet"                       "s"   #'yas-insert-snippet
       :desc "From clipboard"                "y"   #'gwp::yank-dwim
       :desc "Current file name"             "f"   #'+default/insert-file-path
       :desc "Current file path"             "F"   (cmd!! #'+default/insert-file-path t)
       :desc "Evil ex path"                  "p"   (cmd! (evil-ex "R!echo "))
       :desc "From evil register"            "r"   #'evil-ex-registers
       :desc "Unicode"                       "u"   #'insert-char
       ))
;; bc190292 ends here
