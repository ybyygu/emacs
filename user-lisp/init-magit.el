;; [[file:../doom.note::5587631c][5587631c]]
(use-package magit
  :config
  ;; 隐藏untracked文件列表. 更多时候的操作是stage/commit
  (setq magit-section-initial-visibility-alist (quote ((untracked . hide))))
  ;;禁用magit中的gravatars支持, 响应能快一些.
  (setq magit-revision-show-gravatars nil))

(provide 'init-magit)
;; 5587631c ends here
