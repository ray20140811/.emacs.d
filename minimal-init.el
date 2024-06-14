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
