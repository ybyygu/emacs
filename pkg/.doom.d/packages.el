;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.
;;
;; WARNING: Disabling core packages listed in ~/.emacs.d/core/packages.el may
;; have nasty side-effects and is not recommended.


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(unpin! t)

;; ...but to unpin a single package:
;(unpin! pinned-package)
;; Use it to unpin multiple packages
;(unpin! pinned-package another-pinned-package)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

(package! org-pdftools :recipe (:host github :repo "fuxialexander/org-pdftools"))
(package! org-superstar :recipe (:host github :repo "integral-dw/org-superstar-mode"))

;; 五笔输入法
;; (package! pyim-wbdict)

;; (package! berrys-theme)
;; (package! material-theme)
(package! material-theme
          :recipe (:host github :repo "ybyygu/emacs-material-theme"))

(package! org-sidebar)
(package! el-patch)

(package! keyfreq)

(package! org-noter)

(package! nix-mode)

(disable-packages! pangu-spacing)
(disable-packages! pyim)

(package! doom-snippets :ignore t)

(package! forge :ignore t)
;; 不能简单的禁用了事
;; (package! github-review :ignore t)

(package! dired-sidebar)
(package! dockerfile-mode)
(package! yaml-mode)
(package! yaml-mode)
(package! move-dup)

;; https://github.com/org-roam/org-roam-ui#doom
;; 2021-08-01 用不起来
(package! websocket)
(package! simple-httpd)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! org-transclusion
  :recipe (:host github
           :repo "nobiot/org-transclusion"
           :branch "main"
           :files ("*.el")))

;; [[file:../../doom.note::e61c257c][e61c257c]]
(package! golden-ratio)
;; e61c257c ends here

;; [[file:../../doom.note::d6a037d0][d6a037d0]]
(package! fcitx)
(package! rime)
;; d6a037d0 ends here

;; [[file:../../doom.note::1dfc22ab][1dfc22ab]]
(package! symbol-overlay)
;; 1dfc22ab ends here

;; [[file:../../doom.note::*vterm][vterm:1]]
(package! vterm-toggle)
;; vterm:1 ends here

;; [[file:../../doom.note::e86dc54d][e86dc54d]]
(package! cargo)
(package! racer)
(package! rust-mode)
(package! citre)
;; e86dc54d ends here

;; [[file:../../doom.note::*screenshot][screenshot:2]]
(package! org-download)
;; screenshot:2 ends here

;; [[file:../../doom.note::*pairs][pairs:1]]
(package! smartparens-org :ignore t)
;; pairs:1 ends here
