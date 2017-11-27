;; [[file:~/Install/configs/spacemacs/config.note::8494456e-4208-499d-944d-cd14efb43ae7][8494456e-4208-499d-944d-cd14efb43ae7]]
(with-eval-after-load 'org-agenda
  ;; 2013-01-20: less is more
  ;; (setq org-agenda-files (append (file-expand-wildcards "~/Notes/*.note") (file-expand-wildcards "~/Notes/*/*.note")))
  (setq org-agenda-files "~/Notes/.agenda_files")

  (setq org-agenda-include-diary t)
  (setq org-agenda-span (quote month))

  ;; the default is todo-start
  (setq org-icalendar-use-scheduled (quote (event-if-not-todo event-if-todo todo-start)))
  (setq org-icalendar-alarm-time 5)

  (setq org-agenda-custom-commands
        '(("a" agenda "" nil ("~/Notes/org-cal.ics"))
          ("g" . "GTD contexts") ; description for "g" prefix
          )
        )

  (add-to-list 'org-agenda-custom-commands
               '("gr" "Reading"
                 ((tags-todo "Reading|Read"
                             (
                              (org-agenda-overriding-header "待读列表\n------------------")
                              (org-agenda-sorting-strategy '(category-keep priority-down))
                              (org-agenda-remove-tags t)
                              (org-agenda-compact-blocks t)
                              )
                             )
                  (tags "REFILE"
                        (
                         (org-agenda-overriding-header "Tasks to Refile\n------------------")
                         (org-tags-match-list-sublevels nil)
                         )
                        )
                  )
                 ((org-agenda-compact-blocks t))
                 )
               )

  (add-to-list 'org-agenda-custom-commands
               '("gt" "Tasks"
                 ((tags "ASAP+TODO=\"TODO\""
                        (
                         (org-agenda-overriding-header "ASAP\n------------------")
                         (org-agenda-sorting-strategy '(priority-down category-keep timestamp-up))
                         (org-agenda-max-entries 5)
                         )
                        )

                  (tags "Project+Action+TODO=\"TODO\""
                        (
                         (org-agenda-overriding-header "Project\n------------------")
                         (org-agenda-sorting-strategy '(priority-down category-keep timestamp-up))
                         )
                        )
                  (tags "Action+Study+TODO=\"TODO\""
                        (
                         (org-agenda-overriding-header "Topics\n------------------")
                         (org-agenda-files '("~/Notes/research.note"))
                         (org-agenda-sorting-strategy '(priority-down timestamp-up))
                         (org-agenda-max-entries 5)
                         )
                        )

                  (tags "Action+TODO=\"TODO\""
                        (
                         (org-agenda-overriding-header "生活琐事\n------------------")
                         (org-agenda-files '("~/Notes/life.note"))
                         (org-agenda-sorting-strategy '(priority-down timestamp-up))
                         (org-agenda-max-entries 5)
                         )
                        )

                  (tags "Computer+TODO=\"TODO\""
                        (
                         (org-agenda-overriding-header "电脑调优\n------------------")
                         (org-agenda-sorting-strategy '(priority-down timestamp-up))
                         (org-agenda-max-entries 5)
                         )
                        )
                  )
                 ;; options set here apply to the entire block
                 (
                  (org-tags-match-list-sublevels nil)
                  (org-agenda-prefix-format "%-20c ")
                  (org-agenda-todo-keyword-format "")
                  (org-agenda-remove-tags t)
                  (org-agenda-compact-blocks t)
                  )
                 )
               )

  ;; Show all future entries for repeating tasks
  (setq org-agenda-repeating-timestamp-show-all t)
  ;; do not show agenda dates if they are empty
  (setq org-agenda-show-all-dates nil)

  ;; Sorting order for tasks on the agenda
  (setq org-agenda-sorting-strategy
        (quote ((agenda time-up priority-down category-up)
                (todo priority-down)
                (tags priority-down))))

  ;; Start the weekly agenda today
  (setq org-agenda-start-on-weekday nil)
  ;; do not include todo items
  (setq org-agenda-include-all-todo nil)
  )
;; 8494456e-4208-499d-944d-cd14efb43ae7 ends here

;; [[file:~/Install/configs/spacemacs/config.note::9a06a2a8-3a2d-40b0-8701-3999d836f39f][9a06a2a8-3a2d-40b0-8701-3999d836f39f]]
;; disabled for upgrading to org-9.0
;; auto export agenda
;; (defun gwp/run-agenda-store ()
;;   ""
;;   (message "Exporting agenda... ")
;;   (org-batch-store-agenda-views)
;;   (message "Agenda exported."))

;; ;; export agenda if I am away for 2 minutes
;; (run-with-idle-timer 600 t 'gwp/run-agenda-store)
;; 9a06a2a8-3a2d-40b0-8701-3999d836f39f ends here

;; [[file:~/Install/configs/spacemacs/config.note::048aa38e-7f6f-4c9f-94da-020c82ea50e4][048aa38e-7f6f-4c9f-94da-020c82ea50e4]]
(with-eval-after-load 'org-compat

  ;; for org 8
  ;; (org-add-link-type "zotero"
  ;;                    'gwp/org-zotero-open
  ;;                    'gwp/org-zotero-export)

  ;; since org 9
  (org-link-set-parameters "zotero" :follow #'gwp/org-zotero-open :export #'gwp/org-zotero-export)

  (defun gwp/org-zotero-open (path)
    (setq url (format "zotero:%s" path))
    ;; (message-box url)
    (browse-url url)
    )

  (defun gwp/org-zotero-export (path desc format)
    "Create the export version of zotero link specified by PATH and
DESC. FORMATs understood are 'odt','latex and 'html."
    (cond
     ((eq format 'html)
      (format "<a href=\"zotero:%s\">%s</a>" path desc))
     ((eq format 'latex)
      (format "\\href{zotero:%s}{%s}" path desc))
     (t desc)
     )
    )
)
;; 048aa38e-7f6f-4c9f-94da-020c82ea50e4 ends here

;; [[file:~/Install/configs/spacemacs/config.note::08773fc4-f834-41ef-96bd-695b7eb0668e][08773fc4-f834-41ef-96bd-695b7eb0668e]]
(with-eval-after-load 'ob
  ;; activate languages for evaluation
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)                   ;; this is the default
     (R . t)
     (python . t)
     (ipython . t)
     (perl . t)
     (sed . t)
     (shell . t)
     )
   )

  ;; add <p for python expansion
  (add-to-list 'org-structure-template-alist
               '("p" "#+begin_src python\n?\n#+end_src" "<src lang=\"python\">\n?\n</src>"))

  ;; add <el for emacs-lisp expansion
  (add-to-list 'org-structure-template-alist
               '("el" "#+begin_src emacs-lisp\n?\n#+end_src" "<src lang=\"emacs-lisp\">\n?\n</src>"))

  ;; add <b for shell scritp
  (add-to-list 'org-structure-template-alist
               '("b" "#+begin_src shell \n?\n#+end_src" "<src lang=\"shell\">\n?\n</src>"))

 )
;; 08773fc4-f834-41ef-96bd-695b7eb0668e ends here

;; [[file:~/Install/configs/spacemacs/config.note::03a57f74-e2b2-467e-b771-42843b8e1c95][03a57f74-e2b2-467e-b771-42843b8e1c95]]
(defun gwp/org-confirm-babel-evaluate (lang body)
  (not (string= lang "python")))  ; don't ask for python
(setq org-confirm-babel-evaluate 'gwp/org-confirm-babel-evaluate)
;; 03a57f74-e2b2-467e-b771-42843b8e1c95 ends here

;; [[file:~/Install/configs/spacemacs/config.note::39884b8e-6175-4435-88ab-92de1770efde][39884b8e-6175-4435-88ab-92de1770efde]]
;; saving a source code buffer back into its base buffer
(setq org-edit-src-auto-save-idle-delay 1)
;; 39884b8e-6175-4435-88ab-92de1770efde ends here

;; [[file:~/Install/configs/spacemacs/config.note::c5a08df6-7fd7-408b-9fb3-b4eb7347e84e][c5a08df6-7fd7-408b-9fb3-b4eb7347e84e]]
;; unique, memorable identity for tangling
;; (setq org-id-prefix (format-time-string "%Y%m%d"))
;; the default is ok for me
;; (setq org-id-method 'uuid)
;; c5a08df6-7fd7-408b-9fb3-b4eb7347e84e ends here

;; [[file:~/Install/configs/spacemacs/config.note::b5128b51-bf3b-48f7-a96e-51417a752882][b5128b51-bf3b-48f7-a96e-51417a752882]]
;; helper functions for literate programming
;; taking from: https://github.com/grettke/help/blob/master/Org-Mode_Fundamentals.org
(defun help/set-org-babel-default-header-args (property value)
  "Easily set system header arguments in org mode.

PROPERTY is the system-wide value that you would like to modify.

VALUE is the new value you wish to store.

Attribution: URL `http://orgmode.org/manual/System_002dwide-header-arguments.html#System_002dwide-header-arguments'"
  (setq org-babel-default-header-args
        (cons (cons property value)
              (assq-delete-all property org-babel-default-header-args))))

(defun help/set-org-babel-default-inline-header-args (property value)
  "See `help/set-org-babel-default-header-args'; same but for inline header args."
  (setq org-babel-default-inline-header-args
        (cons (cons property value)
              (assq-delete-all property org-babel-default-inline-header-args))))

(defun help/set-org-babel-default-header-args:R (property value)
  "See `help/set-org-babel-default-header-args'; same but for R.

This is a copy and paste. Additional languages would warrant a refactor."
  (setq org-babel-default-header-args:R
        (cons (cons property value)
              (assq-delete-all property org-babel-default-header-args:R))))

(defun help/set-org-babel-default-header-args:ditaa (property value)
  "See `help/set-org-babel-default-header-args'; same but for ditaa.

This is a copy and paste. Additional languages would warrant a refactor."
  (setq org-babel-default-header-args:ditaa
        (cons (cons property value)
              (assq-delete-all property org-babel-default-header-args:ditaa))))

(defun help/set-org-babel-default-header-args:dot (property value)
  "See `help/set-org-babel-default-header-args'; same but for dot.

This is a copy and paste. Additional languages would warrant a refactor."
  (setq org-babel-default-header-args:dot
        (cons (cons property value)
              (assq-delete-all property org-babel-default-header-args:dot))))

(defun help/set-org-babel-default-header-args:plantuml (property value)
  "See `help/set-org-babel-default-header-args'; same but for plantuml.

This is a copy and paste. Additional languages would warrant a refactor."
  (setq org-babel-default-header-args:plantuml
        (cons (cons property value)
              (assq-delete-all property org-babel-default-header-args:plantuml))))

(defun help/org-toggle-macro-markers ()
  (interactive)
  (setq org-hide-macro-markers (not org-hide-macro-markers)))

(defun help/org-prp-hdln ()
  "Visit every Headline. If it doesn't have an ID property then add one and
  assign it a UUID. Attribution: URL
  `http://article.gmane.org/gmane.emacs.orgmode/99738'. It is OK to leave the
  colon separator in here because these are never used as Source-Blocks and
  the rest of the code expects the colon separator."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (dolist (p (nreverse
                (org-element-map (org-element-parse-buffer 'headline) 'headline
                  (lambda (headline) (org-element-property :begin headline)))))
      (goto-char p)
      (org-id-get-create))
    (save-buffer)))

(defun help/org-id-new ()
  "Re-purposing `org-id' hit a snag when colons were forbidden in Source-Block
  names. Adding support for a user-defined Org-Id separator would have fixed
  this but with no benefit to Org-Id. So this function removes the colon
  instead.
 "
  (interactive)
  (let* ((gend (org-id-new))
         (newid (replace-regexp-in-string ":" "_" gend)))
    newid))

(defun help/org-prp-src-blk ()
  "If it doesn't have a NAME property then add one and
   assign it a UUID. Attribution: URL `http://article.gmane.org/gmane.emacs.orgmode/99740'"
  (interactive)
  (help/org-2every-src-block
   #'(lambda (element)
       (if (not (org-element-property :name element))
           (let ((i (org-get-indentation)))
             (beginning-of-line)
             (save-excursion (insert "#+name: " (help/org-id-new) "\n"))
             (indent-to i)
             (forward-line 2))))))

(defconst help/org-special-pre "^\s*#[+]")

(defun help/org-2every-src-block (fn)
  "Visit every Source-Block and evaluate `FN'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward (concat help/org-special-pre "BEGIN_SRC") nil t)
        (let ((element (org-element-at-point)))
          (when (eq (org-element-type element) 'src-block)
            (funcall fn element)))))
    (save-buffer)))

(defun help/org-babel-demarcate-block ()
  "Add a NAME property then assign it a UUID."
  (interactive)
  (org-babel-demarcate-block)
  (insert "#+name: " (help/org-id-new))
  (beginning-of-line)
  (insert "\n"))
;; b5128b51-bf3b-48f7-a96e-51417a752882 ends here

;; [[file:~/Install/configs/spacemacs/config.note::d3151aaf-84cf-484a-9513-40a671de7081][d3151aaf-84cf-484a-9513-40a671de7081]]
;; add name header automatically
(with-eval-after-load "ob-tangle"
  (add-hook 'org-babel-pre-tangle-hook #'help/org-prp-src-blk)
)
;; d3151aaf-84cf-484a-9513-40a671de7081 ends here

;; [[file:~/Install/configs/spacemacs/config.note::a454727c-034e-4967-9193-3beaeb4b984f][a454727c-034e-4967-9193-3beaeb4b984f]]
(help/set-org-babel-default-header-args :padline "yes")
(help/set-org-babel-default-header-args :mkdirp "yes")
(help/set-org-babel-default-header-args :comments "noweb")
;; a454727c-034e-4967-9193-3beaeb4b984f ends here

;; [[file:~/Install/configs/spacemacs/config.note::ec580eca-a4b0-4677-9992-6c62803ce1d7][ec580eca-a4b0-4677-9992-6c62803ce1d7]]
(with-eval-after-load "ob-tangle"
  ;; update timestamps on tangled files
  (setq time-stamp-pattern "100/UPDATED:[ \t]+\\\\?[\"<]+%:y-%02m-%02d %3a %02H:%02M\\\\?[\">]")
  (defun org-babel-post-tangle-hook--time-stamp ()
    "Update timestamps on tangled files."
    (time-stamp)
    (save-buffer))
  (add-hook 'org-babel-post-tangle-hook 'org-babel-post-tangle-hook--time-stamp))
;; ec580eca-a4b0-4677-9992-6c62803ce1d7 ends here

;; [[file:~/Install/configs/spacemacs/config.note::24c35c8a-7587-4c52-bbfb-f7f8a198e292][24c35c8a-7587-4c52-bbfb-f7f8a198e292]]
;; tangle blocks for current file at point
;; http://stackoverflow.com/questions/28727190/org-babel-tangle-only-one-code-block
;; call org-babel-tangle with C-u C-u
(defun gwp/org-babel-tangle-blocks()
  (interactive)
  (let ((current-prefix-arg '(16)))
    (call-interactively 'org-babel-tangle)
    )
  )

(defun gwp/org-edit-save-and-tangle ()
  "when in a sub-editing buffer, swith to the parent buffer and tangle the file blocks"
  (interactive)
  (when (buffer-modified-p) (org-edit-src-save))
  (org-edit-src-exit)
  (call-interactively 'gwp/org-babel-tangle-blocks)
  (org-edit-src-code)
  )

(defun gwp/org-babel-tangle-dwim()
  "tangle current file blocks whenever in a sub-editing buffer or not"
  (interactive)
  (if (org-src-edit-buffer-p) (call-interactively 'gwp/org-edit-save-and-tangle)
    (call-interactively 'gwp/org-babel-tangle-blocks)
    )
  )
;; 24c35c8a-7587-4c52-bbfb-f7f8a198e292 ends here

;; [[file:~/Install/configs/spacemacs/config.note::8934f349-26c4-48a4-945d-5944f3baf2f3][8934f349-26c4-48a4-945d-5944f3baf2f3]]
(setq org-src-fontify-natively nil)
;; 8934f349-26c4-48a4-945d-5944f3baf2f3 ends here

;; [[file:~/Install/configs/spacemacs/config.note::7c756a95-fbc4-4159-b6e7-bc4f3ebf972e][7c756a95-fbc4-4159-b6e7-bc4f3ebf972e]]
(with-eval-after-load 'org-crypt
  (epa-file-enable)

  ;; Encrypt all entries before saving
  (org-crypt-use-before-save-magic)
  (setq org-crypt-tag-matcher "crypt")
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
                                        ; GPG key to use for encryption
  (setq org-crypt-key "38D95BC6411A87E7") ; ybyygu@gmail.com
  (setq org-crypt-disable-auto-save nil)
  )
;; 7c756a95-fbc4-4159-b6e7-bc4f3ebf972e ends here

;; [[file:~/Install/configs/spacemacs/config.note::70ff43af-9dfc-457c-b4b7-e423715cc689][70ff43af-9dfc-457c-b4b7-e423715cc689]]
(with-eval-after-load 'ox
  ;; allow bind variables
  (setq org-export-allow-bind-keywords t)

  ;; disable evaluation when export source codes
  (setq org-export-babel-evaluate nil)

  ;; disabled for updating to version 9.0
  ;; PDFs visited in Org-mode are opened in Evince (and not in the default choice)
  ;; http://stackoverflow.com/a/8836108/789593
  ;; (delete '("\\.pdf\\'" . default) org-file-apps)
  ;; (delete '("\\.djvu\\'" . default) org-file-apps)
  ;; (delete '("\\.png\\'" . default) org-file-apps)
  ;; (delete '("\\.ods\\'" . default) org-file-apps)
  ;; (delete '("\\.doc\\'" . default) org-file-apps)
  ;; (delete '("\\.html\\'" . default) org-file-apps)
  ;; (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))
  ;; (add-to-list 'org-file-apps '("\\.djvu\\'" . "evince %s"))
  ;; (add-to-list 'org-file-apps '("\\.ods\\'" . "libreoffice --calc %s"))
  ;; (add-to-list 'org-file-apps '("\\.doc\\'" . "libreoffice --writer %s"))
  ;; (add-to-list 'org-file-apps '("\\.png\\'" . "eog %s"))
  ;; (add-to-list 'org-file-apps '("\\.html\\'" . "firefox %s"))
  )
;; 70ff43af-9dfc-457c-b4b7-e423715cc689 ends here

;; [[file:~/Install/configs/spacemacs/config.note::f0388b8f-03f1-4caf-9630-3760aba9bb2d][f0388b8f-03f1-4caf-9630-3760aba9bb2d]]
(use-package ox-latex
  :config
  (progn
    (setq org-latex-classes
          (cons '("article"
                  "\\documentclass[11pt,article,oneside]{memoir}"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                org-latex-classes))

    (setq org-latex-classes
          (cons '("cn-article"
                  "\\documentclass[nocap]{ctexart}
                    [NO-DEFAULT-PACKAGES]
                    [NO-PACKAGES]"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                org-latex-classes))

    ;; Use XeLaTeX to export PDF in Org-mode
    (setq org-latex-pdf-process
          '("xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f")
          )
    )
  )
;; f0388b8f-03f1-4caf-9630-3760aba9bb2d ends here

;; [[file:~/Install/configs/spacemacs/config.note::32a6da28-5f14-4433-ac15-6d2749b036f5][32a6da28-5f14-4433-ac15-6d2749b036f5]]
;; bigger latex fragment
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
;; 32a6da28-5f14-4433-ac15-6d2749b036f5 ends here

;; [[file:~/Install/configs/spacemacs/config.note::edc2c0a1-b324-4cf5-8f84-816894cb6c7b][edc2c0a1-b324-4cf5-8f84-816894cb6c7b]]
(setq org-preview-latex-image-directory ".ltximg/")
;; edc2c0a1-b324-4cf5-8f84-816894cb6c7b ends here

;; [[file:~/Install/configs/spacemacs/config.note::7150b085-c305-4285-9fc6-2c5496e0876f][7150b085-c305-4285-9fc6-2c5496e0876f]]
(setq org-capture-templates
      '(
        ("i" "interleave" plain (file "~/annotation.note")
         "#+setupfile: ~/Notes/common.org\n#+ZOTERO_ITEM: %x\n#+INTERLEAVE_PDF: %?\n" :prepend t :kill-buffer t)
        ("n" "Note" entry (file "~/Notes/refile.note")
         "* %u %? [[%:link][%:description]]\n  %:initial\n" :prepend t)
        ("t" "Task" entry (file+headline "~/Notes/life.note" "Tasks")
         "* TODO %^T\n  %i" :prepend t)
        ("r" "Research Memo" entry (file+headline "~/Notes/research.note" "Memo")
         "* %u %?\n  %i\n" :prepend t)
        ("p" "Paper" entry (file+headline "~/Notes/research.note" "References")
         "* %u %? %x\n  %i\n" :prepend t)
        ("j" "Life Journal" entry (file+headline "~/Notes/life.note" "Journals")
         "* %u %?\n  %i\n" :prepend t)
        )
      )
;; 7150b085-c305-4285-9fc6-2c5496e0876f ends here

;; [[file:~/Install/configs/spacemacs/config.note::86d9dfb7-8aed-4cb4-b058-76b456bfe8f6][86d9dfb7-8aed-4cb4-b058-76b456bfe8f6]]
(use-package org-protocol
  :ensure org-plus-contrib
  :demand t
  )
;; 86d9dfb7-8aed-4cb4-b058-76b456bfe8f6 ends here

;; [[file:~/Install/configs/spacemacs/config.note::c38fb49c-9ebf-4103-a404-f52514abc11f][c38fb49c-9ebf-4103-a404-f52514abc11f]]
;; any headline with level <= 2 is a target
(setq org-refile-targets '(
                           (org-agenda-files :tag . "Incoming")
                           )
      )

(setq org-reverse-note-order t)
(defun gwp/get-org-file-link-path ()
  (save-excursion
    (beginning-of-line)
    (search-forward "[[file:" (line-end-position))
    (if (org-in-regexp org-bracket-link-regexp 1)
        (org-link-unescape (match-string-no-properties 1))
      )
    )
  )

(defun gwp/enter-to-read-state()
  "evoke external shell script when entering READ state"
  (when (equal org-state "READ")
    (setq file (gwp/get-org-file-link-path))
    (if file
        (progn
         (setq cmd (concat "org-to-read.sh " (shell-quote-argument file)))
         (message cmd)
         (shell-command cmd)
        )
        )
    )
    (when (equal org-last-state "READ")
      (message "try to remove READ state")
      (setq file (gwp/get-org-file-link-path))
      (if file
          (progn
            (setq cmd (concat "org-read-done.sh " (shell-quote-argument file)))
            (message cmd)
            (shell-command cmd)
            )
        )
      )
  )
(add-hook 'org-after-todo-state-change-hook 'gwp/enter-to-read-state)

;; show a sparse-tree in READ keyword
(defun gwp/org-show-read-tree ()
  "show a sparse-tree in READ keyword"
  (interactive)

  (let ((base-vector [?\C-u ?\M-x ?o ?r ?g ?- ?s ?h ?o ?w ?- ?t ?o ?d ?o ?- ?t ?r ?e ?e return ?R ?E ?A ?D return]))
    ;; create new macro of the form
    ;; C-u M-x org-show-todo-tree RET READ RET
    (execute-kbd-macro (vconcat base-vector
                                (vector 'return)))))
;; c38fb49c-9ebf-4103-a404-f52514abc11f ends here

;; [[file:~/Install/configs/spacemacs/config.note::73c2cba6-d7ba-4692-b09a-d8863a31a338][73c2cba6-d7ba-4692-b09a-d8863a31a338]]
(setq org-attach-store-link-p 'attached)
;; 73c2cba6-d7ba-4692-b09a-d8863a31a338 ends here

;; [[file:~/Install/configs/spacemacs/config.note::bde4a52b-13fe-4932-a274-28ad7cc14ac6][bde4a52b-13fe-4932-a274-28ad7cc14ac6]]
;; 1. store the directory
(defun gwp/org-attach-store (&optional force)
  "store org attachment directory of current enetry"
  (interactive "P")
  ;; make a temporary symlink to store the attachment path
  (setq file-attach-tmp (concat spacemacs-cache-directory ".gwp-attach-tmp"))
  (let ((attach-dir (org-attach-dir)))
    (when attach-dir
      (progn
        ;; remove existing directory
        (when (file-directory-p file-attach-tmp) (delete-directory file-attach-tmp t))
        ;; remove existing file and symlink
        (when (file-exists-p file-attach-tmp) (delete-file file-attach-tmp))
        ;; remove broken symlink
        (when (file-symlink-p file-attach-tmp) (delete-file file-attach-tmp))
        (make-symbolic-link attach-dir file-attach-tmp)
        (message (format "stored to: %s" file-attach-tmp))
        )
      )
    )
  )
;; bde4a52b-13fe-4932-a274-28ad7cc14ac6 ends here

;; [[file:~/Install/configs/spacemacs/config.note::140c8695-f25d-4512-b0f1-1fe4c8edd5c2][140c8695-f25d-4512-b0f1-1fe4c8edd5c2]]
;; 2. move the stored directory to new location
(defun gwp/org-attach-move (&optional force)
  "move stored attachments to current entry"
  (interactive "P")
  ;; ~/.emacs.d/.cache/.gwp-attach-tmp
  (setq file-attach-tmp (concat spacemacs-cache-directory ".gwp-attach-tmp"))

  (if (file-exists-p file-attach-tmp)
      ;; create attachment directory if not exists using org-attach-dir function
      (let ((attach-dir (org-attach-dir t)))
        (progn
          ;; read old attach directory from previous stored symlink
          (setq attach-dir-old (file-chase-links file-attach-tmp))
          ;; sanity check
          (if (y-or-n-p (format "%s/* ==> %s ?" attach-dir-old attach-dir))
              (progn
                (shell-command (format "mv %s/* %s" attach-dir-old attach-dir))
                ;; remove stale tmp-link
                (delete-file file-attach-tmp)
                )
            (message "cancelled")
            )
          )
        )
    (message (format "no stored symbolic link found: %s" file-attach-tmp))
    )
  )
;; 140c8695-f25d-4512-b0f1-1fe4c8edd5c2 ends here

;; [[file:~/Install/configs/spacemacs/config.note::fb26a313-8d58-4e2a-b515-3314d860d44c][fb26a313-8d58-4e2a-b515-3314d860d44c]]
;; inspired by John Kitchin's config: https://emacs.stackexchange.com/a/26627/17011
(defun gwp/take-screenshot ()
  "Minimize emacs, and take a screenshot using org-download"
  (interactive)

  (suspend-frame)
  (org-download-screenshot)
  (raise-frame)
  )

(use-package org-download
  :after org
  :ensure t
  :bind
  (("C-c <insert>" . gwp/take-screenshot)
   ("C-c <delete>" . org-download-delete)
   )
  :config
  (progn
    (setq org-download-method 'attach)
    (setq org-download-image-html-width 900) ; in px
    (setq org-download-image-latex-width 16)  ; in cm
    (setq org-download-screenshot-method "deepin-screenshot -n -s %s 2>/dev/null" )
    )
  )
;; fb26a313-8d58-4e2a-b515-3314d860d44c ends here

;; [[file:~/Install/configs/spacemacs/config.note::4136bddd-2d62-4d66-8f84-aa28e09006ca][4136bddd-2d62-4d66-8f84-aa28e09006ca]]
(require 'org-man)

;; do not commit attachments with git
(setq org-attach-commit nil)
;; 4136bddd-2d62-4d66-8f84-aa28e09006ca ends here
