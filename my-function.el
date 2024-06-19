(defun my-split-window-below (&optional arg)
  "Split the current window 70/30 rather than 50/50.
A single-digit prefix argument gives the top window arg*10%."
  (interactive "P")
  (let ((proportion (* (or arg 7) 0.1)))
    (split-window-below (round (* proportion (window-height))))))

;; font
;(defvar my-font-list '("Andika" "Helvetica" "JetBrains Mono" "IntelOne Mono" "Roboto Mono" "Fira Code" "Lucida Console")
;    "List of fonts to cycle through.")

;; get all font families
;(defvar my-font-list (font-family-list))

(cond
 ((eq system-type 'windows-nt)
  (defvar my-font-list '("Andika" "Helvetica" "JetBrains Mono" "IntelOne Mono" "Roboto Mono" "Fira Code" "Lucida Console")
    "List of fonts to cycle through."))
 ((eq system-type 'gnu/linux)
  (defvar my-font-list (font-family-list))))

(defvar my-current-font-index 0
    "Index of the current font in the font list.")

(defun my-set-next-font ()
    "Set the next font from the font list."
    (interactive)
    (setq my-current-font-index (mod (1+ my-current-font-index) (length my-font-list)))
    (set-frame-font (nth my-current-font-index my-font-list))
    (message "current font: %s" (nth my-current-font-index my-font-list)))

;; theme
(defvar my-theme-list
    '(deeper-blue
        tsdh-dark
        manoj-dark))

(defvar my-current-theme-index 0
    "Index of the current theme in the font list.")

(defun my-next-theme ()
    "Set the next theme from the theme list."
    (interactive)
    (setq my-current-theme-index (mod (1+ my-current-theme-index) (length my-theme-list)))
    (load-theme (nth my-current-theme-index my-theme-list))
    (message "current theme: %s" (car custom-enabled-themes)))


(defun my-get-all-themes ()
    "Get a list of all available themes."
    (interactive)
    (message "Available themes: %s" (custom-available-themes)))

(setq my-theme-list (custom-available-themes))

(defun maximize-frame ()
    "Maximize the current frame."
    (interactive)
    (set-frame-parameter nil 'fullscreen 'maximized))

(defun minimize-frame ()
    "Minimize the current frame."
    (interactive)
    (set-frame-parameter nil 'fullscreen 'minimized))

(defun frame-center ()
  "Center the current frame."
  (interactive)
  (let* ((dw (display-pixel-width))
         (dh (display-pixel-height))
         (f  (selected-frame))
         (fw (frame-pixel-width f))
         (fh (frame-pixel-height f))
         (x  (- (/ dw 2) (/ fw 2)))
         (y  (- (/ dh 2) (/ fh 2))))
    (message (format "dw %d dh %d fw %d fh %d x %d y %d" dw dh fw fh x y))
    (set-frame-position f x y)))

(defun replace-file (source-file target-file)
    "Replace the contents of TARGET-FILE with the contents of SOURCE-FILE."
    (interactive "fSource file: \nfTarget file: ")
    (with-temp-buffer
        (insert-file-contents source-file)
        (write-region (point-min) (point-max) target-file)))    

(defun launch-separate-emacs-in-terminal ()
  (suspend-emacs "fg ; emacs -nw"))

(defun launch-separate-emacs-under-x ()
  ;(call-process "sh" nil nil nil "-c" "emacs &"))
  (call-process "runemacs"))

(defun restart-emacs ()
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
                                                           #'launch-separate-emacs-under-x
                                                         #'launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))        
