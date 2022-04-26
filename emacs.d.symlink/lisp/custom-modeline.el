;; Adapted from https://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html and
;; https://gitlab.com/jessieh/mood-line

;;; Code:

(declare-function projectile-project-p "projectile")
(declare-function projectile-project-name "projectile")
(defun sml/perform-projectile-replacement (in)
  "If path IN is inside a project, use its name as a prefix."
  (let ((proj (projectile-project-p)))
    (if (stringp proj)
        (let* ((replacement
                (format "[%s] "
                        (projectile-project-name)))
               (short (replace-regexp-in-string
                       (concat "^" (regexp-quote (abbreviate-file-name proj)))
                       replacement
                       in)))
          (if (string= short in)
              (let* ((true-in (abbreviate-file-name (file-truename in)))
                     (true-short
                      (replace-regexp-in-string
                       (concat "^" (regexp-quote (abbreviate-file-name (file-truename proj))))
                       replacement true-in)))
                (if (string= true-in true-short) in true-short))
            short))
      in)))

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
  "Decide if we want directory shown. If so, return it."
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


(defun gk-ellipsize-file-or-directory-name (name maxlen)
  "Ellipsize the directory part of a file NAME.
If NAME is larget than MAXLEN, ellipsise the directory part,
preserving, ‘file-name-nondirectory’ if it's a file or the last
directory name if a directory, returning the ellipsized string as
the result."
  (if (> (length name) maxlen)
      (if (or (file-directory-p name)
              (save-match-data (string-match "/$" name)))
          (let* ((bits (split-string name "/" t))
                 (head (butlast bits))
                 (tail (car (last bits))))
            (concat
             (unless (equal (car bits) "~") "/")
             (substring (mapconcat #'identity head "/") 0
                        (- (- maxlen 4) (length bits)))
             ".../" tail "/"))
        (let ((fnod (file-name-nondirectory name)))
          (concat
           (substring (file-name-directory name) 0
                      (- (- maxlen 4) (length fnod)))
           ".../" fnod)))
    name))


(defun mood-line-format (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT, aligned respectively."
  (let ((reserve (length right)))
    (when (and (display-graphic-p) (eq 'right (get-scroll-bar-mode)))
      (setq reserve (- reserve 3)))
    (concat
     left
     " "
     (propertize  " "
                  'display `((space :align-to (- (+ right right-fringe right-margin) ,(+ reserve 0)))))
     right)))

(defface mood-line-status-grayed-out
  '((t (:inherit (font-lock-doc-face))))
  "Face used for neutral or inactive status indicators in the mode-line."
  :group 'mood-line)

(defface mood-line-status-info
  '((t (:inherit (org-level-5))))
  "Face used for generic status indicators in the mode-line."
  :group 'mood-line)

(defface mood-line-status-warning
  '((t (:inherit (org-level-3))))
  "Face for warning status indicators in the mode-line."
  :group 'mood-line)

(defface mood-line-status-error
  '((t (:inherit (org-todo))))
  "Face for error stauts indicators in the mode-line."
  :group 'mood-line)

;; VC update function
(defvar-local mood-line--vc-text nil)
(defun mood-line--update-vc-segment (&rest _)
  "Update `mood-line--vc-text' against the current VCS state."
  (setq mood-line--vc-text
        (when (and vc-mode buffer-file-name)
          (let ((backend (vc-backend buffer-file-name))
                (state (vc-state buffer-file-name (vc-backend buffer-file-name))))
            (let ((face 'mode-line-inactive)
                  (active t))
              (concat (cond ((memq state '(edited added))
                             (if active (setq face 'mood-line-status-info))
                             (propertize "✚" 'face face))
                            ((eq state 'needs-merge)
                             (if active (setq face 'mood-line-status-warning))
                             (propertize "●" 'face face))
                            ((eq state 'needs-update)
                             (if active (setq face 'mood-line-status-warning))
                             (propertize "⬆" 'face face))
                            ((memq state '(removed conflict unregistered))
                             (if active (setq face 'mood-line-status-error))
                             (propertize "✖" 'face face))
                            (t
                             (if active (setq face 'mood-line-status-grayed-out))
                             (propertize "✔" 'face face)))
                      " "
                      (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                                  'face (if active face))
                      "  "))))))

(defun mood-line-segment-vc ()
  "Displays color-coded version control information in the mode-line."
  (if mood-line--vc-text
      (concat " | " mood-line--vc-text)
    " "))

(defvar sml/directory-truncation-string (if (char-displayable-p ?…) "…/" ".../")
  "String used when truncating part of the file path.
Set this to nil or an empty string if you don't want any
indication of a truncated path.")

(defvar sml/prefix-regexp '(":\\(.*:\\)" "~/"))
(defun sml/regexp-composer (getter)
  "Prepare the actual regexp using `sml/prefix-regexp'.
If GETTER is non-nil, result regexp also accepts empty match."
  (let ((left "^\\(")
        (right (if getter "\\|\\).*" "\\)")))
    (if (stringp sml/prefix-regexp)
        (if (string-match "\\(" sml/prefix-regexp)
            sml/prefix-regexp
          (concat left sml/prefix-regexp right))
      (concat left (mapconcat 'identity sml/prefix-regexp "\\|") right))))

(defun sml/strip-prefix (path)
  "Remove prefix from string PATH.
A prefix is anything at the beginning of the line that matches a
regexp in `sml/prefix-regexp'."
  (replace-regexp-in-string (sml/regexp-composer nil) "" path))

(defun sml/do-shorten-directory (dir max-length)
  "Show up to MAX-LENGTH characters of a directory name DIR."
  (let ((longname (sml/strip-prefix dir)))
    ;; If it fits, return the string.
    (if (<= (string-width longname) max-length) longname
      ;; If it doesn't, shorten it
      (let ((path (reverse (split-string longname "/")))
            (output ""))
        (when (and path (equal "" (car path)))
          (setq path (cdr path)))
        (let ((max (- max-length (string-width sml/directory-truncation-string))))
          ;; Concat as many levels as possible, leaving 4 chars for safety.
          (while (and path (<= (string-width (concat (car path) "/" output))
                               max))
            (setq output (concat (car path) "/" output))
            (setq path (cdr path))))
        ;; If we had to shorten, prepend .../
        (when path
          (setq output (concat sml/directory-truncation-string output)))
        output))))

(setq-default mode-line-format
              '((:eval
                 (mood-line-format
                  ;; Left
                  (format-mode-line
                   (list
                    ;; is this buffer read-only?
                    '(:eval (when buffer-read-only
                              (concat (propertize " READ-ONLY "
                                                  'face 'org-todo
                                                  'help-echo "Buffer is read-only"))))

                    ;; relative position
                    ;; "("
                    ;; (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
                    ;; ")"

                    '(:eval (propertize (concat (sml/perform-projectile-replacement (sml/get-directory))
                                                (sml/do-shorten-directory (sml/buffer-name) 40)
                                                " ")
                                        'face 'font-lock-constant-face
                                        ;; 'help-echo (if buffer-file-name (abbreviate-file-name buffer-file-name) nil)
                                        ))

                    ;; the buffer name; the file name as a tool tip
                    ;; '(:eval (propertize (concat (abbreviate-file-name buffer-file-name) " ") 'face 'font-lock-constant-face
                    ;;                     'help-echo (buffer-file-name)))
                    ))

                  ;; Right
                  (format-mode-line
                   (list

                    mode-line-misc-info

                    " |"

                    (tab-bar-format-tabs)

                    ;; the current major mode for the buffer.
                    '(:eval (let ((mode (if (stringp mode-name) mode-name (-first-item mode-name))))
                              (if mode
                                  (concat " | "
                                          (propertize mode
                                                      'face 'font-lock-variable-name-face
                                                      'help-echo buffer-file-coding-system))
                                nil)))

                    '(:eval (mood-line-segment-vc))
                    ;; (string-trim (lsp-mode-line))
                    )
                   )))))

(add-hook 'find-file-hook #'mood-line--update-vc-segment)
(add-hook 'after-save-hook #'mood-line--update-vc-segment)
(advice-add #'vc-refresh-state :after #'mood-line--update-vc-segment)

(provide 'hbournis/modeline)
;;; modeline.el ends here
