(load-file "~/.emacs.d/common.el")
(load-file "~/.emacs.d/tip-of-the-day.el")
(load-file "~/.emacs.d/my-function.el")
(load-file "~/.emacs.d/my-style.el")

(setq-default mode-line-format nil)

(setq default-frame-alist '((undecorated . t)))

(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 10))

(global-linum-mode)

(menu-bar-mode 0)

(tool-bar-mode 0)

(scroll-bar-mode 0)

(frame-center)
