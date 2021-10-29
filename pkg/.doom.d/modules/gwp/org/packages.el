;; [[file:../../../../../doom.note::c98fa262][c98fa262]]
(package! org)
(package! websocket)
(package! evil-org)
(package! evil-org-agenda :recipe (:host github :repo "Somelauw/evil-org-mode"))
(package! org-roam)
(package! org-roam-ui)
(package! org-superstar)
(package! org-pdftools)
(package! org-download)
(package! org-noter)
(package! orgit)
(package! ob-ipython)
(package! org-sidebar)

;; for http api hacking
(package! restclient)
(package! ob-restclient)

;; (package! org-z :recipe (:host github :repo "landakram/org-z"))
;; (package! org-z-selectrum :recipe (:host github :repo "landakram/org-z"))

(package! org-contrib
  :recipe (:host nil :repo "https://git.sr.ht/~bzg/org-contrib"))

;; for org-babel gnuplot
(package! gnuplot-mode)
(package! gnuplot)
;; c98fa262 ends here
