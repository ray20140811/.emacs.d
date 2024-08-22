;; Reference: https://realpython.com/emacs-the-best-python-editor

;; ===================================
;; MELPA Package Support
;; ===================================
;; Enables basic packaging support
(require 'package)

;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Initializes the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; Installs packages
;;
;; myPackages contains a list of package names
(defvar myPackages
  '(better-defaults
;    elpy
    flycheck
    material-theme
    )
  )

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)

;; ===================================
;; Basic Customization
;; ===================================

(setq inhibit-startup-message t)    ;; Hide the startup message
(load-theme 'material t)            ;; Load material theme
(global-linum-mode t)               ;; Enable line numbers globally

;; ====================================
;; Development Setup
;; ====================================
;; Enable elpy
;(elpy-enable)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; ===================================
;; My Configuration 
;; ===================================
(set-face-attribute 'default nil :height 140)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

(setq-default cursor-type 'bar)

(tool-bar-mode 0)

(cond
 ((eq system-type 'windows-nt)
  (setq default-directory "D:/"))
 ((eq system-type 'gnu/linux)
  (setq default-directory "~/")))

(cond
 ((eq system-type 'windows-nt)
  (when (member "JetBrains Mono" (font-family-list))
    (set-frame-font "JetBrains Mono" t t)))
 ((eq system-type 'gnu/linux)
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-frame-font "DejaVu Sans Mono" t t))))

(global-flycheck-mode t)

;(setq python-shell-interpreter "D:/Python/Python312/python.exe")

;; 狀態列顯示日期時間
(setq display-time-day-and-date t)

;; 設置時間顯示格式
(setq display-time-format "%Y-%m-%d")
(display-time-mode 1)


;(defun open-today-org-file ()
;  "Open an .org file named with today's date in the format YYYY-MM-DD."
;  (let ((filename (format-time-string "%Y-%m-%d.org")))
;    (find-file (expand-file-name filename "D:/Daily"))))

;(add-hook 'emacs-startup-hook 'open-today-org-file)

;(open-today-org-file)

;; User-Defined init.el ends here


