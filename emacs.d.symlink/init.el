;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename:      $HOME/.emacs.d/init.el
;; Time-stamp:    <2017-09-22 13:12:00 karl.voit>
;; Source:        https://github.com/novoid/dot-emacs
;; Purpose:       configuration file for Emacs
;; Authors:       Karl Voit
;; License:       This file is licensed under the GPL v2.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file was originally created by Karl Voit and can be found at https://github.com/novoid/dot-emacs/blob/master/init.el.
;; I removed some comments and the code that outputs how long the procedure took.

;; set paths to manually installed Org-mode (from git; instead of built-in Org-mode)
;; (add-to-list 'load-path "~/.emacs.d/contrib/org-mode/lisp")
;; (add-to-list 'load-path "~/.emacs.d/contrib/org-mode/contrib/lisp" t)
;; (require 'org)

;; set correct user-emacs-directory on Windows


(setq gc-cons-threshold 100000000)

(setq read-process-output-max (* 1024 1024)) ; 1mb (needed for lsp mode)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq my-user-emacs-directory "~/.emacs.d/")

(defun for-correct-platform-p (keyword)
  "Return t if KEYWORD match the check for the current platform."
  (pcase system-type
    ('windows-nt (member keyword '("WINDOWS_ONLY" "NOT_MAC" "NOT_UNIX" "NOT_LINUX")))
    ('darwin     (member keyword '("MAC_ONLY" "UNIX_ONLY" "NOT_WINDOWS" "NOT_LINUX")))
    ('gnu/linux  (member keyword '("LINUX_ONLY" "UNIX_ONLY" "NOT_WINDOWS" "NOT_MAC")))))

(defun block-info-valid-p (tfile)
  "Return t if TFILE is not 'no'."
  (not (string= "no" tfile)))

(defun emacs-lisp-code-block-p (lang)
  "Return t if LANG is emacs-lisp."
  (string= "emacs-lisp" lang))

(defun todo-keyword-valid-p (todo-keyword)
  "Return t if TODO-KEYWORD valid."
  (let ((work? (if (getenv "WORK") t nil)))
    (pcase todo-keyword
      (`nil t)
      ("DISABLED" nil)
      ("WORK_ONLY" work?)
      ("NOT_WORK" (not work?))
      ((or "" "TODO" "WAITING") t)
      ((pred for-correct-platform-p) t))))

;; =======================================================================================
;; The init.el file looks for "config.org" and tangles its elisp blocks (matching
;; the criteria described below) to "config.el" which is loaded as Emacs configuration.
;; Inspired and copied from: http://www.holgerschurig.de/en/emacs-init-tangle/
;; =======================================================================================

(defun my-tangle-config-org ()
  "This function will write all source blocks from =config.org= into =config.el= that are:
- not marked as =tangle: no=
- doesn't have the TODO state =DISABLED=
- have a source-code of =emacs-lisp="
  (require 'org)
  (let* ((body-list ())
         (output-file (concat my-user-emacs-directory "init-tangled.el"))
         (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
                                                                (list (cons :tangle output-file)))))
    (message "—————• Re-generating %s …" output-file)
    ;; save-restriction
    ;; save-excursion
    (save-restriction
      (save-excursion
        ;; If I set it to ~/.emacs.d/init.org it does not recognize that it is the same
        ;; file because it is sylinked, and it closes it on each save.
        (org-babel-map-src-blocks "~/.dotfiles/emacs.d.symlink/init.org" ;; (concat my-user-emacs-directory "init.org")
          (let* (
                 (org_block_info (org-babel-get-src-block-info 'light))
                 (tfile (cdr (assq :tangle (nth 2 org_block_info))))
                 (match_for_TODO_keyword)
                 )
            ;; save-excursion
            (catch 'exit
              (org-back-to-heading t)
              (when (looking-at org-outline-regexp)
                (goto-char (1- (match-end 0))))
              (when (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
                (setq match_for_TODO_keyword (match-string 1))))
            (if (and (block-info-valid-p tfile)
                     (emacs-lisp-code-block-p lang)
                     (todo-keyword-valid-p match_for_TODO_keyword)
                     )
                (add-to-list 'body-list (concat "\n\n;; #####################################################################################\n"
                                                ";; • "
                                                (org-get-heading)
                                                "\n\n"
                                                body)
                             )
              )))))

    (with-temp-file output-file
      (insert ";; ============================================================\n")
      (insert ";; Don't edit this file, edit config.org' instead ...\n")
      (insert ";; ============================================================\n\n")
      (insert (apply 'concat (reverse body-list))))
    (message "—————• Wrote %s" output-file)
    ))


;; following lines are executed only when my-tangle-config-org-hook-func()
;; was not invoked when saving config.org which is the normal case:
(let ((orgfile (concat my-user-emacs-directory "init.org"))
      (elfile (concat my-user-emacs-directory "init-tangled.el"))
      (gc-cons-threshold most-positive-fixnum))
  (when (or (not (file-exists-p elfile))
            (file-newer-than-file-p orgfile elfile))
    (my-tangle-config-org))
  (load-file elfile))

;; when config.org is saved, re-generate config.el:
(defun my-tangle-config-org-hook-func ()
  (when (string= "init.org" (buffer-name))
    (let ((orgfile (concat my-user-emacs-directory "init.org"))
          (elfile (concat my-user-emacs-directory "init-tangled.el")))
      (my-tangle-config-org))))
(add-hook 'after-save-hook 'my-tangle-config-org-hook-func)
