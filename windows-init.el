(require 'package)

(package-initialize)

(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;(setq package-archives '(("gnu" . "~/.emacs.d/elpa/gnu/")
;			 ("nongnu" . "~/.emacs.d/elpa/nongnu/")
;			 ("melpa" . "~/.emacs.d/melpa/")
;			 ("org" . "~/.emacs.d/elpa/org/")))


(setq default-directory "D:/Ray/")        

(setq inhibit-startup-screen t)

(load-theme 'deeper-blue)

(display-time-mode 1)
(global-linum-mode)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq-default cursor-type 'bar)
(setq-default mode-line-format nil)

(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 10))

(setq default-frame-alist '((undecorated . t)))

(set-frame-font "Andika" t)

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

