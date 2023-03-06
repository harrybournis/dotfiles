(setq package-enable-at-startup nil
      frame-inhibit-implied-resize t
      inhibit-startup-message t
      load-prefer-newer noninteractive
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      default-frame-alist '((menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars)))

;; max memory available for gc on startup
;; (defvar me/gc-cons-threshold 16777216)

;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq gc-cons-threshold me/gc-cons-threshold
;;                   gc-cons-percentage 0.1)))

;; (setq site-run-file nil)

(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

(provide 'early-init)
;;; early-init.el ends here
