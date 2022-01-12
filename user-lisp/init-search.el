;; [[file:../gwp.note::6194c32a][6194c32a]]
(defun gwp::zoxide-recent-directories ()
  (let* ((output (shell-command-to-string "zoxide query --list"))
         (dirs (split-string output "[\r\n]+" t)))
    dirs))

(defun gwp::dired-recent-directories ()
  (let* ((recent-dirs
          (mapcar (lambda (file)
                    (if (file-directory-p file) file (file-name-directory file)))
                  recentf-list)))
    recent-dirs))

(defun gwp::zoxide-add-directory (dir)
  "将 dir 加入 zoxide 数据库中"
  (when dir (call-process "zoxide" nil nil nil "add" dir)))

;; open recent directory, requires ivy (part of swiper)
;; borrows from http://stackoverflow.com/questions/23328037/in-emacs-how-to-maintain-a-list-of-recent-directories
;;;###autoload
(defun gwp::ivy-recent-dirs ()
  "Present a list of recently used directories and open the selected one in dired"
  (interactive)
  (let ((recent-dirs
         (delete-dups
          (append (gwp::zoxide-recent-directories) (gwp::dired-recent-directories)))))
    (let ((dir (ivy-read "Directory: " recent-dirs
                         :sort nil
                         :action '(1 ("o" gwp::zoxide-add-directory "open")))))
      (dired dir))))

(provide 'init-search)
;; 6194c32a ends here
