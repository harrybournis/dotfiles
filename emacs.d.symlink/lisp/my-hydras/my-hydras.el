;;; my-hydras.el --- Custom
;;; Commentary:
;;; Code:
(defhydra unpackaged/smerge-hydra
  (:color pink :hint nil :post (smerge-auto-leave))
  "
  ^Move^       ^Keep^               ^Diff^                 ^Other^
  ^^-----------^^-------------------^^---------------------^^-------
  _n_ext       _b_ase               _<_: upper/base        _C_ombine
  _p_rev       _u_pper              _=_: upper/lower       _r_esolve
  ^^           _l_ower              _>_: base/lower        _k_ill current
  ^^           _a_ll                _R_efine
  ^^           _RET_: current       _E_diff
  "
  ("n" smerge-next)
  ("p" smerge-prev)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("\C-m" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("R" smerge-refine)
  ("E" smerge-ediff)
  ("C" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("k" smerge-kill-current)
  ("ZZ" (lambda ()
          (interactive)
          (save-buffer)
          (bury-buffer))
   "Save and bury buffer" :color blue)
  ("q" nil "cancel" :color blue))

(defhydra zoom (global-map "<f2>")
  "zoom"
  ("j" hbournis/increment-font-size "in")
  ("k" hbournis/decrement-font-size  "out"))

(defhydra hydra-global-org (:color blue)
  "Org"
  ("t" org-timer-start "Start Timer")
  ("s" org-timer-stop "Stop Timer")
  ("r" org-timer-set-timer "Set Timer") ; This one requires you be in an orgmode doc, as it sets the timer for the header
  ("p" org-timer "Print Timer") ; output timer value to buffer
  ("w" (org-clock-in '(4)) "Clock-In") ; used with (org-clock-persistence-insinuate) (setq org-clock-persist t)
  ("o" org-clock-out "Clock-Out") ; you might also want (setq org-log-note-clock-out t)
  ("j" org-clock-goto "Clock Goto") ; global visit the clocked task
  ("c" org-capture "Capture") ; Don't forget to define the captures you want http://orgmode.org/manual/Capture.html
  ("l" org-capture-goto-last-stored "Last Capture"))

(defhydra hydra-window-deluxe-custom (global-map "<f3>")
  "
  Move^^  ^Resize^ ^Split^          ^Switch^
  --------------------------------------------
  _h_ ←   _H_ X←   _v_ertical       _b_uffer
  _j_ ↓   _J_ X↓   _x_ horizontal   _f_ind
  _k_ ↑   _K_ X↑                    _d_elete
  _l_ →   _L_ X→
  "
  ("h" windmove-left )
  ("j" windmove-down )
  ("k" windmove-up )
  ("l" windmove-right )
  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)
  ("b" helm-mini)
  ("f" helm-find-files)
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right))
   )
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down))
   )
  ("d" delete-window))

(provide 'my-hydras)
;;; my-hydras.el ends here
