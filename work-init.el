(require 'package)

(package-initialize)

(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;(setq package-archives '(("gnu" . "~/.emacs.d/elpa/gnu/")
;			 ("nongnu" . "~/.emacs.d/elpa/nongnu/")
;			 ("melpa" . "~/.emacs.d/melpa/")
;			 ("org" . "~/.emacs.d/elpa/org/")))

(set-frame-font "Andika" t)

(setq default-directory "D:/Ray/")        

(setq inhibit-startup-screen t)

(load-theme 'deeper-blue)

(display-time-mode 1)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq-default cursor-type 'bar)
(setq-default mode-line-format nil)

(setq default-frame-alist '((undecorated . t)))

(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 10))


;; Set UTF-8 as the default encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

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

(frame-center)

(global-linum-mode)

(defun my-split-window-below (&optional arg)
  "Split the current window 70/30 rather than 50/50.
A single-digit prefix argument gives the top window arg*10%."
  (interactive "P")
  (let ((proportion (* (or arg 7) 0.1)))
    (split-window-below (round (* proportion (window-height))))))

(global-set-key (kbd "C-x 2") 'my-split-window-below)