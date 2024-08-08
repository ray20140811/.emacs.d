(defun vscode-style ()
  (interactive)
  (if (file-exists-p "~/.emacs.d/init.el")
    (progn
      (setq timestamp (format-time-string "%Y%m%d%H%M%S"))
      (rename-file "~/.emacs.d/init.el" (concat "~/.emacs.d/init-" timestamp ".el"))))
  (copy-file "~/.emacs.d/vscode-init.el" "~/.emacs.d/init.el" t t)
  (restart-emacs))

(defun windows-style ()
  (interactive)
  (if (file-exists-p "~/.emacs.d/init.el")
    (progn
      (setq timestamp (format-time-string "%Y%m%d%H%M%S"))
      (rename-file "~/.emacs.d/init.el" (concat "~/.emacs.d/init-" timestamp ".el"))))
  (copy-file "~/.emacs.d/windows-init.el" "~/.emacs.d/init.el" t t)
  (restart-emacs))
	
(defun real-python-style ()
  (interactive)
  (if (file-exists-p "~/.emacs.d/init.el")
    (progn
      (setq timestamp (format-time-string "%Y%m%d%H%M%S"))
      (rename-file "~/.emacs.d/init.el" (concat "~/.emacs.d/init-" timestamp ".el"))))
  (copy-file "~/.emacs.d/real-python-init.el" "~/.emacs.d/init.el" t t)
  (restart-emacs))	

