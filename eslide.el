;; eslide
; (setq eslide-edit-mode-map (make-sparse-keymap))
(defvar eslide-control-map
  (let ((m (make-sparse-keymap)))
    (loop for (key . command) in
          '(("n"   . eslide-next)
            ("SPC" . eslide-next)
            ("p"   . eslide-prev)
            ("DEL" . eslide-prev))
          do (define-key m (macroexpand `(kbd ,key)) command))
    m))

(defconst +eslide-separator+ "^----$")
(defvar eslide-slide-source nil)
(defvar eslide-current-slide-overlay nil)

(defun eslide-get-buffers nil
  "Return a cons cell containing the display buffer and the message buffer."
  (loop for i in (list (get-buffer-create "*ESlide Show*")
                       (get-buffer-create "*ESlide Notes*"))
        do (with-current-buffer i (use-local-map eslide-control-map))
        collect i))

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
  (eslide-get-buffers)
  (eslide-next))

(define-derived-mode eslide-edit-mode fundamental-mode "ESlide[Edit]"
  "Edit eslides"
  (setq font-lock-keywords (list +eslide-separator+))
  (define-key eslide-edit-mode-map (kbd "C-c C-c") #'eslide-start)
  (define-key eslide-edit-mode-map (kbd "C-c C-n") #'eslide-next)
  (define-key eslide-edit-mode-map (kbd "C-c C-p") #'eslide-prev))

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
      (insert (apply #'format (cons (concat format "\n") args)))
      (goto-char (point-max)))))

(defun eslide-show-note (format &rest args)
  (destructuring-bind (show notes) (eslide-get-buffers)
    (apply #'eslide--append-to-buffer (list* notes format args))
    (loop for win in (get-buffer-window-list notes)
          do (set-window-point win (with-current-buffer notes (point-max))))))

(defun eslide-show-slide (text)
  (destructuring-bind (show notes) (eslide-get-buffers)
    (with-current-buffer show
      (delete-region (point-min) (point-max))
      (insert text))))

(defun eslide-move (direction)
  "Move next slide in positive or negative DIRECTION"
  (with-current-buffer eslide-slide-source
    (save-excursion
      (cond ((< direction 0)
             (goto-char (overlay-start eslide-current-slide-overlay))
             (ignore-errors (skip-chars-backward "-\n")))
            ((> direction 0)
             (goto-char (overlay-end eslide-current-slide-overlay))
             (ignore-errors (skip-chars-forward "-\n"))))
      (eslide-update-current-slide (point)))
    (let ((text (eslide-get-current-slide-text)))
      (eslide-show-note "current: %s" text)
      (eslide-show-slide text))))

(defun eslide-next nil
  (interactive)
  (eslide-move 1))

(defun eslide-prev nil
  (interactive)
  (eslide-move -1))
