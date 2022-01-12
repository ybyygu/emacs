;; [[file:../../../gwp.note::e86dc54d][e86dc54d]]
(package! magit)
(package! magit-todos)
(package! cargo)
(package! racer)
(package! rust-mode)
(package! citre)
(package! find-file-in-project)
;; 2021-10-26 上游更新所致 https://github.com/magit/git-modes
;; 这两个包安装报错, 临时禁用
(package! gitconfig-mode :ignore t)
(package! gitignore-mode :ignore t)

(package! block-nav :recipe (:host github :repo "nixin72/block-nav.el"))
;; e86dc54d ends here
