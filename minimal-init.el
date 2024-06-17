(require 'package)

(package-initialize)

(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;(setq package-archives '(("gnu" . "~/.emacs.d/elpa/gnu/")
;			 ("nongnu" . "~/.emacs.d/elpa/nongnu/")
;			 ("melpa" . "~/.emacs.d/melpa/")
;			 ("org" . "~/.emacs.d/elpa/org/")))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

(setq inhibit-startup-screen t)

(load-theme 'deeper-blue)

(setq-default cursor-type 'bar)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; set default font
(cond
 ((eq system-type 'windows-nt)
  (when (member "Andika" (font-family-list))
    (set-frame-font "Andika" t t)))
 ((eq system-type 'gnu/linux)
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-frame-font "DejaVu Sans Mono" t t))))

;; set default directory
(cond
 ((eq system-type 'windows-nt)
  (setq default-directory "D:/Ray/"))
 ((eq system-type 'gnu/linux)
  (setq default-directory "~/")))

(display-time-mode 1)  