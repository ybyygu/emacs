;; [[file:../../../../../doom.note::d6a037d0][d6a037d0]]
;; (package! fcitx)
(package! rime)
(package! golden-ratio)
; (package! pyim)
;; 五笔输入法
;; (package! pyim-wbdict)

;; pangu-spacing 自动给中英文字加空格, 这严重影响响应速度.
(disable-packages! pangu-spacing)
;; 避免与rime的设置有冲突
(disable-packages! pyim)
;; d6a037d0 ends here
