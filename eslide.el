;; eslide
; (setq eslide-edit-mode-map (make-sparse-keymap))
(defvar eslide-control-map
  (let ((m (make-sparse-keymap)))
    (loop for (key . command) in
          `(("n"       . eslide-next)
            ("SPC"     . eslide-next)
            ("<right>" . eslide-next)
            ("p"       . eslide-prev)
            ("DEL"     . eslide-prev)
            ("<left>"  . eslide-prev)
            ("<prior>" . ,(lambda () (interactive) (scroll-down 1)))
            ("<next>" . ,(lambda () (interactive) (scroll-up 1)))
            ("+" . eslide-text-scale-increase)
            ("=" . eslide-text-scale-increase)
            ("<XF86AudioRaiseVolume>" . eslide-text-scale-increase)
            ("-" . eslide-text-scale-decrease)
            ("_" . eslide-text-scale-decrease)
            ("<XF86AudioLowerVolume>" . eslide-text-scale-decrease))
          do (define-key m (macroexpand `(kbd ,key)) command))
    m))

(defconst +eslide-separator+ "^----\n")
(defvar eslide-slide-source nil)
(defvar eslide-current-slide-overlay nil)
(defvar eslide-start-time nil)

(defface eslide-code '((t
                        (:underline "green" :background "grey20"))) "indented code face")

(defvar eslide-buffers
  (loop for i in (list (get-buffer-create "*ESlide Show*")
                       (get-buffer-create "*ESlide Notes*"))
        do (with-current-buffer i
             (use-local-map eslide-control-map)
             (setq buffer-read-only t))
        collect i))

(defmacro build-with-<foo>-macro (macro-name buffer-getter-form)
  `(defmacro ,macro-name (&body body)
     `(with-current-buffer ,,buffer-getter-form ,@body)))

(macroexpand '(build-with-<foo>-macro with-show-buffer (car eslide-buffers)))


(with-current-buffer (car eslide-buffers)
  (buffer-face-set '(:height 2.0)))

(defun eslide-start nil
  "Start the slideshow using the current buffer as the slide source"
  (interactive)
  (setq eslide-slide-source (current-buffer))
  (goto-char (point-min))
  (when eslide-current-slide-overlay
    (delete-overlay eslide-current-slide-overlay))
  (setq eslide-current-slide-overlay
        (make-overlay (point-min) (point-min) eslide-slide-source))
  (overlay-put eslide-current-slide-overlay 'face '(:background "#040"))
  eslide-buffers
  (setq eslide-start-time (cadr (current-time)))
  (eslide-next))

(define-derived-mode eslide-edit-mode fundamental-mode "ESlide[Edit]"
  "Edit eslides"
  (setq font-lock-defaults (list
     (list +eslide-separator+
           '("^\s{4}" . eslide-code-face))
     t))
  (define-key eslide-edit-mode-map (kbd "C-c C-c") #'eslide-start)
  (define-key eslide-edit-mode-map (kbd "C-c C-n") #'eslide-next)
  (define-key eslide-edit-mode-map (kbd "C-c C-p") #'eslide-prev))

(eval-at-startup
  (add-to-list 'auto-mode-alist (cons ".esl$" 'eslide-edit-mode)))

(defun eslide-get-slide-near (pos)
  (save-excursion
    (goto-char pos)
    (let ((start (save-excursion (or
                                  (and
                                   (re-search-backward +eslide-separator+ nil t)
                                   (match-end 0))
                                  (point-min))))
          (end (save-excursion (or
                                (and
                                 (re-search-forward +eslide-separator+ nil t)
                                 (match-beginning 0))
                                (point-max)))))
      (cons start end))))

(defun eslide-update-current-slide (&optional pos)
  (with-current-buffer eslide-slide-source
    (save-excursion
      (goto-char (or pos (overlay-start eslide-current-slide-overlay)))
      (skip-chars-forward "-")
      (destructuring-bind (start . end)
          (eslide-get-slide-near (point))
        (move-overlay eslide-current-slide-overlay start end)))))

(defun eslide-get-current-slide-text nil
  (with-current-buffer eslide-slide-source
    (eslide-update-current-slide)
    (buffer-substring-no-properties
     (overlay-start eslide-current-slide-overlay)
     (overlay-end eslide-current-slide-overlay))))

(defun eslide--append-to-buffer (buffer format &rest args)
  "Add message to the end of BUFFER"
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (apply #'format (cons (concat format "\n") args))))
      (goto-char (point-max)))))

(defun eslide-show-note (format &rest args)
  (destructuring-bind (show notes) eslide-buffers
    (apply #'eslide--append-to-buffer (list* notes format args))
    (with-current-buffer notes
      (loop for win in (get-buffer-window-list notes nil t)
            do (set-window-point win (point-max)))
      (goto-char (point-max))
      (recenter -1))))

(defun eslide-show-slide (text)
  (destructuring-bind (show notes) eslide-buffers
    (with-current-buffer show
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))
        (insert text)
        (loop for win in (get-buffer-window-list show nil t)
              do (set-window-point win (point-max)))
        (goto-char (point-max))
        (recenter -1)))))

(defmacro with-string-buffer (string &rest forms)
  `(with-temp-buffer
     (insert ,string)
     (unwind-protect (progn ,@forms)
       (set-buffer-modified-p nil))
     (buffer-substring (point-min) (point-max))))

(defun eslide-format-note (note face)
  (with-string-buffer
   (propertize note 'face (list :inherit face))
   (goto-char (point-min))
   (insert  "\n")
   (ignore-errors
     (while (< (point) (point-max))
       (beginning-of-line)
       (insert "  ")
       (forward-line)
       (end-of-line)))))

(defun fontify-code (code)
  (with-string-buffer code
      (setq buffer-file-name "/tmp/eslide")
      (cperl-mode)
      (font-lock-fontify-buffer)
      (setq buffer-file-name nil)))

(defconst +code-start+ "  ")

(defun format-one-chunk nil
  (when (progn (beginning-of-line) (looking-at +code-start+))
    (let ((start (point)) (end (save-excursion (end-of-line) (point))) (flag t))
      (ignore-errors
        (while flag
          ;(message "at %d %d" start end)
          (forward-line)
          (if (progn
                (beginning-of-line)
                (looking-at +code-start+))
              (setq end (progn (end-of-line) (point)))
            (setq flag nil))))
      (let ((code (fontify-code (buffer-substring start end))))
        (goto-char start)
        (delete-region start end)
        (insert code)
;        (put-text-property start (point) 'face '(fixed-pitch)))))
)))
  (forward-line))

(defun eslide-format-slide (slide)
  (with-string-buffer slide
    (goto-char (point-min))
    (ignore-errors (while t
                     (format-one-chunk)))))

(defun eslide-move (direction)
  "Move next slide in positive or negative DIRECTION."
  (with-current-buffer eslide-slide-source

    ;; find slide
    (save-excursion
      (cond ((< direction 0)
             (goto-char (overlay-start eslide-current-slide-overlay))
             (ignore-errors (skip-chars-backward "-\n")))
            ((> direction 0)
             (goto-char (overlay-end eslide-current-slide-overlay))
             (ignore-errors (skip-chars-forward "-\n"))))
      (eslide-update-current-slide (point)))

    (with-current-buffer (cadr eslide-buffers)
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))))

    ;; show time in notes window
    (eslide-show-note "time: %s"
      (eslide-format-note
       (format "%d" (/ (- (cadr (current-time)) eslide-start-time) 60))
       'font-lock-warning-face))

    ;; show current/next slide
    (let ((text (eslide-get-current-slide-text)))
      (eslide-show-note "current: %s"
                        (eslide-format-note text
                                            'font-lock-keyword-face))
      (eslide-show-slide (eslide-format-slide text)))
    (let ((next
           (destructuring-bind (start . end)
               (eslide-get-slide-near
                (with-current-buffer eslide-slide-source
                  (save-excursion
                    (goto-char (overlay-end eslide-current-slide-overlay))
                    (skip-chars-forward "-\n ")
                    (point))))
             (buffer-substring-no-properties start end))))
      (eslide-show-note "next: %s"
                        (eslide-format-note next
                                            'font-lock-type-face)))))

(defun eslide-next nil
  (interactive)
  (eslide-move 1))

(defun eslide-prev nil
  (interactive)
  (eslide-move -1))

(defun eslide-text-scale-increase nil
  (interactive)
  (with-current-buffer (car eslide-buffers)
    (text-scale-increase 1)))

(defun eslide-text-scale-decrease nil
  (interactive)
  (with-current-buffer (car eslide-buffers)
    (text-scale-decrease 1)))

(provide 'eslide)
