;;; custom-modeline.el --- Custom
;;; Commentary:
;; Adapted from https://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html and
;; https://gitlab.com/jessieh/mood-line

;;; Code:
(setq hbournis/modeline-buffer-name-length 40)

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
  (let ((projectile-root (if (fboundp 'projectile-project-root) (projectile-project-root) nil))
        (max-length hbournis/modeline-buffer-name-length))
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
       max-length))))

(defun hbournis/mode-line-major-mode ()
  "Return the major mode of the buffer."
  (let ((mode (if (stringp mode-name) mode-name (car mode-name))))
    (when mode
      (propertize (concat " " mode) 'face 'font-lock-variable-name-face))))

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
  (let ((reserve (+ 1 (length right))))
    (concat
     left
     (propertize " " 'display `((space :align-to (- right ,reserve))))
     right)))

(setq-default mode-line-format
              '(:eval
                (hbournis/mode-line-format
                 ;; Left
                 (format-mode-line
                  (list
                   (list 'buffer-read-only (propertize " READ-ONLY " 'face 'org-todo))
                   (propertize (hbournis/mode-line-buffer-name) 'face 'font-lock-constant-face)))
                 ;; Right
                 (format-mode-line
                  (list
                   mode-line-misc-info
                   (tab-bar-format-tabs)
                   (hbournis/mode-line-major-mode)
                   (hbournis/mode-line-git)
                   )))))

(add-hook 'find-file-hook #'hbournis/mode-line-update-vc-segment)
(add-hook 'after-save-hook #'hbournis/mode-line-update-vc-segment)
(advice-add #'vc-refresh-state :after #'hbournis/mode-line-update-vc-segment)

(provide 'custom-modeline)
;;; custom-modeline.el ends here
