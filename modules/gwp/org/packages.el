;; [[file:../../../gwp.note::bec6705d][bec6705d]]
;; (package! org)
;; take from here
;; https://github.com/doomemacs/doomemacs/blob/master/modules/lang/org/packages.el
(package! org
  :recipe (:host github
           ;; REVIEW I intentionally avoid git.savannah.gnu.org because of SSL
           ;;   issues (see #5655), uptime issues, download time, and lack of
           ;;   shallow clone support.
           :repo "emacs-straight/org-mode"
           :files (:defaults "etc")
           :depth 1
           ;; HACK Org requires a post-install compilation step to generate a
           ;;   org-version.el with org-release and org-git-version functions,
           ;;   using a 'git describe ...' call.  This won't work in a sparse
           ;;   clone and I value smaller network burdens on users over
           ;;   non-essential variables so we fake it:
           :build t
           :pre-build
           (with-temp-file "org-version.el"
             (let ((version
                    (with-temp-buffer
                      (insert-file-contents (doom-path "lisp/org.el") nil 0 1024)
                      (if (re-search-forward "^;; Version: \\([^\n-]+\\)" nil t)
                          (match-string-no-properties 1)
                        "Unknown"))))
               (insert (format "(defun org-release () %S)\n" version)
                       (format "(defun org-git-version (&rest _) \"%s-??-%s\")\n"
                               version (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
                       "(provide 'org-version)\n"))))
  :pin "1c7acb427ffc7094a8a7fd37871dfec6e9d36cc8")

(package! org-superstar)
(package! org-pdftools)
(package! org-download)
(package! org-noter)
(package! org-sidebar)
(package! el-patch)
(package! smartparens-org :ignore t)

;; for http api hacking
(package! restclient)
(package! ob-restclient)

(package! org-contrib
  :recipe (:host nil :repo "https://git.sr.ht/~bzg/org-contrib"))

;; for org-babel gnuplot
(package! gnuplot-mode)
(package! gnuplot)
;; bec6705d ends here
