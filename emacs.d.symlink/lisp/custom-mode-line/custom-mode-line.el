;;; custom-modeline.el --- Custom
;;; Commentary:
;; Adapted from https://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html and
;; https://gitlab.com/jessieh/mood-line

;;; Code:
(defvar hbournis/mode-line-buffer-name-length 40)

(defvar hbournis/mode-line-original-format mode-line-format)

(declare-function projectile-project-name "ext:projectile")

(defface hbournis/mode-line-buffer-name-face
  '((t (:inherit (font-lock-constant-face))))
  "Face used for neutral or inactive status indicators in the mode-line."
  :group 'custom-mode-line)

(defface hbournis/mode-line-status-grayed-out
  '((t (:inherit (font-lock-doc-face))))
  "Face used for neutral or inactive status indicators in the mode-line."
  :group 'custom-mode-line)

(defface hbournis/mode-line-status-info
  '((t (:inherit (org-level-5))))
  "Face used for generic status indicators in the mode-line."
  :group 'custom-mode-line)

(defface hbournis/mode-line-status-warning
  '((t (:inherit (org-level-3))))
  "Face for warning status indicators in the mode-line."
  :group 'custom-mode-line)

(defface hbournis/mode-line-status-error
  '((t (:inherit (org-todo))))
  "Face for error stauts indicators in the mode-line."
  :group 'custom-mode-line)

(defun sml/buffer-name ()
  "Return either buffer name or file name to be shown on the mode-line.
Uses `sml/show-file-name' to decide between the two.
Unless `sml/show-trailing-N' is nil, prevents the \"<N>\" (used in
duplicated buffer names) from being displayed."
  (cond ((buffer-base-buffer)
         (buffer-name))
        ((buffer-file-name)
         (file-name-nondirectory (buffer-file-name)))
        ((derived-mode-p 'dired-mode)
         (file-name-nondirectory (directory-file-name default-directory)))
        (t (buffer-name))))

(defun sml/get-directory ()
  "Decide if we want directory shown.  If so, return it."
  (abbreviate-file-name
   (cond
    ;; In email attachments, buffer-file-name is non-nil, but
    ;; file-name-directory returns nil
    ((buffer-file-name) (or (file-name-directory (buffer-file-name)) ""))
    ((eq major-mode 'dired-mode)
     (replace-regexp-in-string "/[^/]*/$" "/" default-directory))
    ((and (symbolp major-mode)
          (member major-mode '(shell-mode eshell-mode term-mode)))
     default-directory)
    ;; In indirect buffers, buffer-file-name is nil. The correct value is
    ;; retrieved from the base buffer.
    ((buffer-base-buffer)
     (with-current-buffer (buffer-base-buffer) (sml/get-directory)))
    (t ""))))

(defun hbournis/abbreviate-long-file-name (file-name max-length)
  "Abbreviate FILE-NAME if its length is more than MAX-LENGTH.
Keep the last MAX-LENGTH characters intact."
  (if (> (length file-name) max-length)
      (concat "..."
              (substring file-name (- (length file-name) max-length)))
    file-name))

(defun hbournis/mode-line-buffer-name ()
  "Return the buffer or file name with the project for the mode-line."
  (condition-case err
      (let ((projectile-root (if (fboundp 'projectile-project-root) (projectile-project-root) nil))
            (max-length hbournis/mode-line-buffer-name-length))
        (if projectile-root
            (concat
             "["
             (projectile-project-name)
             "] "
             (hbournis/abbreviate-long-file-name
              (string-remove-prefix projectile-root (buffer-file-name))
              max-length))
          (hbournis/abbreviate-long-file-name
           (concat (sml/get-directory) (sml/buffer-name))
           max-length)))
    (error (message (error-message-string err)) "BUFFER NAME ERROR")))

(defun hbournis/mode-line-tab-bar ()
  (when (display-graphic-p) (tab-bar-format-tabs)))

(defun hbournis/mode-line-major-mode ()
  "Return the major mode of the buffer."
  (let ((mode mode-name))
    (when mode
      (propertize (concat " " (format-mode-line mode)) 'face 'font-lock-variable-name-face))))

(defvar-local hbournis/mode-line-git-text nil)

(defun hbournis/mode-line-git ()
  "Update git segment."
  (when hbournis/mode-line-git-text
    hbournis/mode-line-git-text))

(defun hbournis/mode-line-update-vc-segment ()
  "Update `hbournis/mode-line-vc-text'."
  (setq hbournis/mode-line-git-text
        (when (and vc-mode buffer-file-name)
          (let ((state (vc-state buffer-file-name (vc-backend buffer-file-name))))
            (propertize
             (concat " \ue725 " (substring-no-properties vc-mode 5))
             'face
             (cond ((memq state '(edited added)) 'hbournis/mode-line-status-info)
                   ((eq state 'needs-merge) 'hbournis/mode-line-status-warning)
                   ((eq state 'needs-update) 'hbournis/mode-line-status-warning)
                   ((memq state '(removed conflict unregistered)) 'hbournis/mode-line-status-error)
                   (t 'hbournis/mode-line-status-grayed-out)))))))

(defun hbournis/mode-line-format (left right)
  "Format LEFT, RIGHT and space in-between."
  (format (format "%%s %%%ds" (- (window-total-width) (length left) 2))
          left
          right))

(add-hook 'find-file-hook #'hbournis/mode-line-update-vc-segment)
(add-hook 'after-save-hook #'hbournis/mode-line-update-vc-segment)
(advice-add #'vc-refresh-state :after #'hbournis/mode-line-update-vc-segment)

;;;###autoload
(define-minor-mode custom-mode-line-mode
  "Custom mode-line."
  :init-value nil
  :keymap nil
  :lighter ""
  :group 'custom-mode-line
  :global t
  (if custom-mode-line-mode
      (progn
        (setq-default mode-line-format
                      '(:eval
                        (hbournis/mode-line-format
                         ;; Left
                         (format-mode-line
                          (list
                           (list 'buffer-read-only (propertize " READ-ONLY " 'face 'org-todo))
                           (propertize (hbournis/mode-line-buffer-name) 'face 'hbournis/mode-line-buffer-name-face)))
                         ;; Right
                         (format-mode-line
                          (list
                           mode-line-misc-info
                           (hbournis/mode-line-tab-bar)
                           (hbournis/mode-line-major-mode)
                           (hbournis/mode-line-git))))))

        (add-hook 'find-file-hook #'hbournis/mode-line-update-vc-segment)
        (add-hook 'after-save-hook #'hbournis/mode-line-update-vc-segment)
        (advice-add #'vc-refresh-state :after #'hbournis/mode-line-update-vc-segment))
    (progn
      (setq-default mode-line-format hbournis/mode-line-original-format)

      (remove-hook 'find-file-hook #'hbournis/mode-line-update-vc-segment)
      (remove-hook 'after-save-hook #'hbournis/mode-line-update-vc-segment)
      (advice-remove #'vc-refresh-state #'hbournis/mode-line-update-vc-segment))))

(provide 'custom-mode-line)
;;; custom-mode-line.el ends here
