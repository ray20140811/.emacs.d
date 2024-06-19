(defun vscode-style ()
  (interactive)
  (copy-file "~/.emacs.d/vscode-init.el" "~/.emacs.d/init.el" t t)
  (restart-emacs))

(defun windows-style ()
  (interactive)
  (copy-file "~/.emacs.d/windows-init.el" "~/.emacs.d/init.el" t t)
  (restart-emacs))


