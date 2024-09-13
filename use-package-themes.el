;; ===================================
;; download use-package
;; ===================================
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; ===================================
;; download themse
;; ===================================
;; doom-themes
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (load-theme 'doom-one t))

(use-package base16-theme :ensure t)
(use-package doom-themes :ensure t)
(use-package dracula-theme :ensure t)
(use-package ef-themes :ensure t)
(use-package gruvbox-theme :ensure t)
(use-package monokai-theme :ensure t)
(use-package one-themes :ensure t)
(use-package solarized-theme :ensure t)
(use-package vscode-dark-plus-theme :ensure t)

;; ==============================================
;; load theme
;; ==============================================

;; (load-theme 'base16-default-dark t)
;; (load-theme 'base16-tomorrow-night t)
;; (load-theme 'doom-one t)
;; (load-theme 'ef-cyprus t)
;; (load-theme 'gruvbox-dark-medium t)
;; (load-theme 'monokai t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'solarized-light t)
