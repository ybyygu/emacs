;; [[file:../../../gwp.note::d6a037d0][d6a037d0]]
(package! winner)
(package! ace-window)
(package! symbol-overlay)
(package! burly :recipe (:host github :repo "alphapapa/burly.el"))
(package! rime)
(package! bm)
(package! golden-ratio)

;; pangu-spacing 自动给中英文字加空格, 这严重影响响应速度.
(disable-packages! pangu-spacing)
;; 避免与rime的设置有冲突
(disable-packages! pyim)
;; d6a037d0 ends here
