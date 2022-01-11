;; [[file:../../../gwp.note::46f457b2][46f457b2]]
(map! :leader
      :desc "window/frame" "w" gwp::window-map
      )
;; 46f457b2 ends here

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
(map! :leader
      "desc" "highlight symbols"
      "H" gwp::symbol-overlay-map)
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
