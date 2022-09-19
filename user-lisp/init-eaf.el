;; [[file:../gwp.note::7d6d2068][7d6d2068]]
(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :after org
  :config
  ;; (require 'eaf-file-manager)
  (require 'eaf-browser)
  (require 'eaf-image-viewer)
  (require 'eaf-pdf-viewer)
  (require 'eaf-demo)
  (require 'eaf-org)
  ;; (require 'eaf-interleave)
  ;; (require 'eaf-terminal)
  ;; (require 'eaf-evil)

  ;; 按键绑定等
  (defalias 'browse-web #'eaf-open-browser)
  (eaf-bind-key close_buffer "q" eaf-pdf-viewer-keybinding)
  (eaf-bind-key add_annot_highlight "d"  eaf-pdf-viewer-keybinding)
  (eaf-bind-key add_annot_underline "a" eaf-pdf-viewer-keybinding)
  (eaf-bind-key add_annot_squiggly "s" eaf-pdf-viewer-keybinding)
  (eaf-bind-key undo_annot_action "u" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "SPC" eaf-pdf-viewer-keybinding) ;; unbind, see more in the Wiki
  (eaf-bind-key zoom_in "=" eaf-pdf-viewer-keybinding)
  (eaf-bind-key zoom_out "-" eaf-pdf-viewer-keybinding)
  (eaf-bind-key jump_to_page "p" eaf-pdf-viewer-keybinding)
  ;; (define-key eaf-mode-map* (kbd "C-c B") #'eaf-open-bookmark)


  (defun eaf-org-open-file (file &optional link)
    "An wrapper function on `eaf-open'."
    (eaf-open file))

  ;; use `emacs-application-framework' to open PDF file: link
  (add-to-list 'org-file-apps '("\\.pdf\\'" . eaf-org-open-file))

  ;; 修复 evil 按键问题. 这里需要启用 eaf-browser 支持, 不然会报错
  ;; (require 'eaf-evil)
  ;; (define-key key-translation-map (kbd "SPC")
  ;;   (lambda (prompt)
  ;;     (if (derived-mode-p 'eaf-mode)
  ;;         (pcase eaf--buffer-app-name
  ;;           ("pdf-viewer" (kbd eaf-evil-leader-key))
  ;;           (_  (kbd "SPC")))
  ;;       (kbd "SPC"))))

  :custom
  ;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  ;; (eaf-browser-continue-where-left-off t)
  ;; (eaf-browser-enable-adblocker t)
  (eaf-pdf-dark-mode nil)
  (confirm-kill-processes nil)	; 退出不需要确认杀死进程
  ;; (browse-url-browser-function 'eaf-open-browser)
  )

(provide 'init-eaf)
;; 7d6d2068 ends here
