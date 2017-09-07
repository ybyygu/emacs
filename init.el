;; install
;; 官方首页提供了三种安装方式. 迁移的时候考虑使用第二种或第三种.

;; 1. spacemacs 目录放置到~/etc/spacemacs/.emacs.d
;;    : git clone https://github.com/syl20bnr/spacemacs.git ~/etc/spacemacs/.emacs.d

;; 2. 修改配置文件中的路径
;;    elpa 默认仍是存在~/.emacs.d/elpa 下, 可以用 [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Files.html][package-user-dir]] 修改.

;;    #+header: :tangle init.el

(setq spacemacs-start-directory "~/Install/configs/spacemacs/.emacs.d/")
(setq package-user-dir (file-name-as-directory
                        (concat spacemacs-start-directory "elpa/")))
(load-file (concat spacemacs-start-directory "init.el"))
