(defun my-split-window-below (&optional arg)
  "Split the current window 70/30 rather than 50/50.
A single-digit prefix argument gives the top window arg*10%."
  (interactive "P")
  (let ((proportion (* (or arg 7) 0.1)))
    (split-window-below (round (* proportion (window-height))))))

;; font
(defvar my-font-list '("Andika" "Helvetica" "JetBrains Mono" "IntelOne Mono" "Roboto Mono" "Fira Code" "Lucida Console")
    "List of fonts to cycle through.")

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

