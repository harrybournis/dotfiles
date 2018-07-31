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
;; (setq gc-cons-threshold 1600000)
;; (setq gc-cons-threshold 50000000)
;; (setq gc-cons-threshold 4000000)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq my-user-emacs-directory "~/.emacs.d/")

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (eq system-type 'windows-nt))

(defun for-wrong-platform-p (keyword)
  "Return non-nil if the KEYWORD does not match the current platform.
Return nil if the keyword is not one of the specified platform keywords"
  (let ((thing nil)
        (windows-only '("WINDOWS_ONLY"))
        (mac-only '("MAC_ONLY" "UNIX_ONLY"))
        (linux-only '("LINUX_ONLY" "UNIX_ONLY"))
        (not-windows '("NOT_WINDOWS"))
        (not-mac '("NOT_MAC" "NOT_UNIX"))
        (not-linux '("NOT_LINUX" "NOT_UNIX"))
        )
    (if (member keyword (append windows-only mac-only linux-only))
        (progn
          (if IS-WINDOWS (setq thing windows-only))
          (if IS-MAC (setq thing mac-only))
          (if IS-LINUX (setq thing linux-only))

          (if (member keyword thing)
              nil t))
      (if (member keyword (append not-windows not-mac not-linux))
          (progn
            (if IS-WINDOWS (setq thing not-windows))
            (if IS-MAC (setq thing not-mac))
            (if IS-LINUX (setq thing not-linux))

            (if (member keyword thing)
                t nil))
          nil))))

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
           (unless (or (string= "no" tfile)
                       (string= "DISABLED" match_for_TODO_keyword)
                       (not (string= "emacs-lisp" lang))
                       (for-wrong-platform-p match_for_TODO_keyword))
             (add-to-list 'body-list (concat "\n\n;; #####################################################################################\n"
                                             ";; • " (org-get-heading) "\n\n")
                          )
             (add-to-list 'body-list body)
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
