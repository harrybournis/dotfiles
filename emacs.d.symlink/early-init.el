(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (eq system-type 'windows-nt))
(defconst WORK?      (if (getenv "WORK") t nil))

(setq package-enable-at-startup nil
      frame-inhibit-implied-resize t
      inhibit-startup-message t
      inhibit-splash-screen t
      inhibit-startup-screen t
      load-prefer-newer noninteractive
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      default-frame-alist '((menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (tab-bar-lines . 0)
                            (undecorated . t)
                            (vertical-scroll-bars)))

;; menu bar hack from doom emacs
(when IS-MAC
  (add-hook 'after-make-frame-functions
            (defun hbournis/set-menu-bar-for-frame (&optional frame)
              (set-frame-parameter frame 'menu-bar-lines
                                   (if (display-graphic-p frame) 1 0)))))

(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

(provide 'early-init)
;;; early-init.el ends here
