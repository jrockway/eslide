;; eslide
; (setq eslide-edit-mode-map (make-sparse-keymap))

(defgroup eslide nil "Eslide")

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
            ("<XF86AudioLowerVolume>" . eslide-text-scale-decrease)
            )
          do (define-key m (macroexpand `(kbd ,key)) command))
    m))

(defconst +eslide-separator+ "^----\n")
(defvar eslide-slide-source nil)
(defvar eslide-current-slide-overlay nil)
(defvar eslide-start-time nil)

(defface eslide-code '((t
                        (:underline "green" :background "grey20"))) "indented code face")

(defvar eslide-buffers nil)

(defun eslide-post-command-hook ()
  (if (equal (buffer-name (current-buffer)) "*ESlide Show*")
      (internal-show-cursor nil nil)
    (internal-show-cursor nil t)))

(add-hook 'post-command-hook #'eslide-post-command-hook)

(defun eslide-ensure-buffers (&optional rebuild)
  "Setup the variable `eslide-buffers'.
Optional argument REBUILD controls rebuilding buffers even when they already exist."
  (when (not (reduce (lambda (&optional a b) (and (boundp a) (boundp b) a b))
                    (loop for buf in eslide-buffers collect (buffer-live-p buf))))
    (setf eslide-buffers nil))
  (when (or (not eslide-buffers) rebuild)
    (setf eslide-buffers
          (loop for i in (list (get-buffer-create "*ESlide Show*")
                               (get-buffer-create "*ESlide Notes*"))
                do (with-current-buffer i
                     (use-local-map eslide-control-map)
                     (setq buffer-read-only t))
                collect i))

    (with-current-buffer (car eslide-buffers)
      (variable-pitch-mode 1))))

(defun eslide-buffers (&optional which) ;; the which is a lie.
  "Return all eslide buffers.
Optional argument WHICH controls which buffers to return and in what order; currently \"show\" and \"notes\" are known."
  (eslide-ensure-buffers)
  eslide-buffers)

(defun eslide-notes ()
  (eslide-ensure-buffers)
  (cadr eslide-buffers))

(defun eslide-show ()
  (eslide-ensure-buffers)
  (car eslide-buffers))

(defmacro build-with-<foo>-macro (macro-name buffer-getter-form)
  `(defmacro ,macro-name (&body body)
     `(with-current-buffer ,,buffer-getter-form ,@body)))

(defun eslide-start nil
  "Start the slideshow using the current buffer as the slide source."
  (interactive)
  (eslide-ensure-buffers)
  (setq eslide-slide-source (current-buffer))
  (goto-char (point-min))
  (when eslide-current-slide-overlay
    (delete-overlay eslide-current-slide-overlay))
  (setq eslide-current-slide-overlay
        (make-overlay (point-min) (point-min) eslide-slide-source))
  (overlay-put eslide-current-slide-overlay 'face '(:background "#040"))
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
    (let ((start (save-excursion
                   (or (and (re-search-backward +eslide-separator+ nil t) (match-end 0))
                       (point-min))))
          (end (save-excursion
                 (or (and (re-search-forward +eslide-separator+ nil t) (match-beginning 0))
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
  (destructuring-bind (show notes) (eslide-buffers '(show notes))
    (apply #'eslide--append-to-buffer (list* notes format args))
    (with-current-buffer notes
      (loop for win in (get-buffer-window-list notes nil t)
            do (set-window-point win (point-max)))
      (goto-char (point-max))
      (recenter -1))))

(defun eslide-show-slide (text)
  (with-current-buffer (eslide-show)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (insert text)
      (text-scale-increase 0)
      (text-scale-increase (eslide-scale-for-slide text))
      (loop for win in (get-buffer-window-list (eslide-show) nil t)
            do (set-window-point win (point-max)))
      (goto-char (point-min)))))

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

(defun format-one-chunk ()
  (cond ((progn (beginning-of-line) (looking-at +code-start+))
         (let ((start (point)) (end (save-excursion (end-of-line) (point))) done)
           (while (and (not done) (= 0 (forward-line)))
             (if (progn (beginning-of-line) (looking-at +code-start+))
                 (setq end (progn (end-of-line) (point)))
               (setq done t)))
           (let ((code (fontify-code (buffer-substring start end))))
             (goto-char start)
             (delete-region start end)
             (insert code)
             ;(put-text-property start (point) 'face '(fixed-pitch)))))
             )))
        (t nil)))

(defun eslide-format-slide (slide)
  "Format the slide SLIDE."
  (with-string-buffer slide
    (goto-char (point-min))
    (while (= 0 (forward-line)) (format-one-chunk))))

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

    (with-current-buffer (eslide-notes)
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
  (with-current-buffer (eslide-show)
    (text-scale-increase 1)))

(defun eslide-text-scale-decrease nil
  (interactive)
  (with-current-buffer (eslide-show)
    (text-scale-decrease 1)))

(defun eslide-narrowest-window (buffer)
  (car (get-buffer-window-list buffer nil t))) ;;; XXX not really

(defun eslide-shortest-window (buffer)
  (car (get-buffer-window-list buffer nil t))) ;;; XXX not really

(defun eslide-text-scale (size)
  (text-scale-increase 0)
  (text-scale-increase size))

(defmacro* with-eslide-temp-show-buffer (&body forms)
  "Execute FORMS in a temp buffer (with special eslide settings in effect)."
  (declare (indent 0))
  `(with-temp-buffer
     (variable-pitch-mode 1)
     ,@forms))

(defun eslide-buffer-fits-on-one-screen-p (window)
  "Return T if current buffer when displayed on WINDOW will not need to scroll."
  (save-window-excursion
    (set-window-buffer window (current-buffer))
    (ignore-errors (while t (scroll-down)))
    (goto-char (point-min))
    (block nil
      (condition-case e (scroll-up)
        (error (return t)))
      (return nil))))

(defun eslide-no-lines-wrap-p (window)
  "Return T if no lines in current buffer wrap when displayed in WINDOW."
  (with-selected-window window
    (= (count-lines (point-min) (point-max))
       (count-screen-lines (point-min) (point-max) nil window))))

(defun* eslide-maximizing-text-scale (predicate window &optional (max 30))
  "Increase text-scale until PREDICATE returns NIL, return max scale where PREDICATE returned nil.  Operate on text of BUFFER as if it were displayed in WINDOW."
  (or (loop for scale from -10 to max
            do (eslide-text-scale scale)
            when (not (funcall predicate window))
            return (1- scale)) max))

(defun eslide-scale-for-slide (slide)
  "Return the maximum text-scale value for SLIDE where no lines wrap and where every line is visible in every ESlide Show window."
  (with-eslide-temp-show-buffer
    (insert slide)
    (variable-pitch-mode)
    (let* ((show (eslide-show))
           (narrowest (eslide-narrowest-window show))
           (shortest (eslide-shortest-window show))
           (max-size-width (eslide-maximizing-text-scale #'eslide-no-lines-wrap-p narrowest)))
      (eslide-maximizing-text-scale #'eslide-buffer-fits-on-one-screen-p shortest max-size-width))))

(provide 'eslide)
