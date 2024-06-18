(require 'package)

(package-initialize)

;(setq package-archives '(("org" . "https://orgmode.org/elpa/")
;                         ("gnu" . "https://elpa.gnu.org/packages/")
;                         ("melpa" . "https://melpa.org/packages/")))

(setq package-archives '(("gnu" . "~/.emacs.d/elpa/gnu/")
			 ("nongnu" . "~/.emacs.d/elpa/nongnu/")
			 ("melpa" . "~/.emacs.d/melpa/")
			 ("org" . "~/.emacs.d/elpa/org/")))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

(setq inhibit-startup-screen t)

(load-theme 'deeper-blue)

(setq-default cursor-type 'bar)

(tool-bar-mode 0)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (set-frame-font "Andika" t)

;; Set UTF-8 as the default encoding
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

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

(load-file "~/.emacs.d/tip-of-the-day.el")
(load-file "~/.emacs.d/my-function.el")

;(split-window-below 10)

(global-set-key (kbd "C-x 2") 'my-split-window-below)
(global-set-key (kbd "<f5>") 'my-set-next-font)
(global-set-key (kbd "<f6>") 'tip-of-the-day)
(global-set-key (kbd "<f7>") 'my-next-theme)

(my-split-window-below)

(eshell)

(window-swap-states)

(neotree)
