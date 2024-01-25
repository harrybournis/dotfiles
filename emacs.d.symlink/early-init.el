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
                            (vertical-scroll-bars)))

(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

(provide 'early-init)
;;; early-init.el ends here
