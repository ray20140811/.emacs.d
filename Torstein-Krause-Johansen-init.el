;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;;           Torstein Krause Johansen's .emacs file                         ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 00 Table of contents
;;
;; Evaluate it with C-x C-e / eval-last-sexp to get toc in its own buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (occur "^;; [0-9]+")

;; Don't show the warnings buffer just because some package has used a
;; deprecated API
(setq warning-minimum-level :error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 01 Emacs package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

;; Put all Emacs customize variables & faces in its own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 02 Name and email
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Torstein Krause Johansen"
      user-mail-address "torstein@skybert.net")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10 Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(menu-bar-mode 0)
(setq column-number-mode t)

(when window-system
  (setq ring-bell-function 'ignore)
  (set-fringe-style 0)
  (set-cursor-color "red")
  (set-scroll-bar-mode nil)
  (tool-bar-mode 0)
  (pixel-scroll-precision-mode 1)
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height 100
                      :weight 'medium
                      :width 'normal))

(use-package sweet-theme
  :ensure t
  :init
  (load-theme 'sweet t))

(use-package doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 20 Global shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigate between buffers
(global-set-key "\M-p" 'previous-buffer)
(global-set-key "\M-n" 'next-buffer)

;; Navigate between visible buffers (windows in emacs speak)
(defun other-window-backward (&optional n)
  (interactive "p")
  (if n
      (other-window (- n))
    (other-frame -1)))
(global-set-key "\C-x\C-n" 'other-window)
(global-set-key "\C-x\C-p" 'other-window-backward)

;; Quickly switch between open buffer windows
(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-dispatch-always t))

;; Give a pulse light when switching windows, or switching focus to
;; the minibuffer.
(require 'pulse)
(set-face-attribute 'pulse-highlight-start-face nil :background "#49505f")
(add-hook 'window-selection-change-functions
          (lambda (frame)
            (when (eq frame (selected-frame))
              (pulse-momentary-highlight-one-line))))

;; Don't report all saves
(setq save-silently t)

;; Balance window splits automatically
(setf window-combination-resize t)

;; Revert
(global-set-key  [ (f5) ] 'revert-buffer-quick)
(global-auto-revert-mode 1)
(setq revert-without-query (list "\\.png$" "\\.svg$")
      auto-revert-verbose nil)

(global-set-key "\M- " 'hippie-expand)
(global-set-key "\M-r" 'join-line)

;; Minimising & quitting Emacs way too many times without wanting to.
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-c")

;; Don't write backslashed to indicate continuous lines
(set-display-table-slot standard-display-table 'wrap ?\ )

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key "\C-x\C-c" 'compile)

;; Mark ring navigation: C-u C-space to jump to the first mark, then
;; C-space to the consecutive ones.
(setq set-mark-command-repeat-pop t)

(use-package multiple-cursors
  :ensure t
  :bind
  ("C->" . 'mc/mark-next-like-this))

;; Minibuffer and buffer navigation
(use-package counsel
  :ensure t
  :bind
  ("C-."   . 'counsel-imenu)
  ("C-c '" . 'projectile-grep)
  ("C-c ," . 'counsel-imenu)
  ("C-h f" . 'counsel-describe-function)
  ("C-h v" . 'counsel-describe-variable)
  ("C-o"   . 'counsel-outline)
  ("C-x b" . 'counsel-switch-buffer))

(use-package smex
  :ensure t
  :bind
  ("M-x" . smex))

(use-package ido-vertical-mode
  :ensure t
  :init
  (setq ido-vertical-indicator ">"
        ido-vertical-define-keys 'C-n-C-p-up-and-down)
  )

;; Sub word support
(add-hook 'minibuffer-setup-hook 'subword-mode)

;; Find file at point
(global-set-key "\C-\M-f" 'find-file-at-point)


(use-package dumb-jump
  :ensure t
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)

  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package ivy
  :ensure t)

(use-package ivy-xref
  :ensure t
  :init
  (setq xref-show-definitions-function #'ivy-xref-show-defs)
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 21 Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-cn" 'find-dired)
(global-set-key "\C-cN" 'grep-find)

;; Show current and total count
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")

(use-package grep
  :ensure t
  :config
  (setq grep-find-ignored-directories
        (append
         (list
          ".git"
          ".hg"
          ".idea"
          ".project"
          ".settings"
          ".svn"
          "bootstrap*"
          "pyenv"
          "target"
          )
         grep-find-ignored-directories))
  (setq grep-find-ignored-files
        (append
         (list
          "*.blob"
          "*.class"
          "*.gz"
          "*.jar"
          "*.xd"
          ".factorypath"
          "TAGS"
          "dependency-reduced-pom.xml"
          "projectile.cache"
          "workbench.xmi"
          )
         grep-find-ignored-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 23 Occur
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(advice-add 'isearch-occur :after
            '(lambda (origin &rest args)
               (isearch-exit)
               (select-window (get-buffer-window "*Occur*"))
               (goto-char (point-min))))

(advice-add 'occur :after
            '(lambda (origin &rest args)
               (isearch-exit)
               (select-window (get-buffer-window "*Occur*"))
               (goto-char (point-min))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 25 Reading & writing files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tell emacs to skip backup files
(setq make-backup-files nil)
;; Yes, I want large files
(setq large-file-warning-threshold 150000000)

;; Rename current buffer, as well as doing the related version control
;; commands to rename the file.
(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message
           "File '%s' successfully renamed to '%s'"
           filename
           (file-name-nondirectory new-name))))))))
(global-set-key (kbd "C-x C-r") 'rename-this-buffer-and-file)

;; Follow symlinks, just tell me about it
(setq vc-follow-symlinks t)

(defun tkj/copy-vc-link()
  "Copies the corresponding Bitbucket web URI of the current buffer
and line number."
  (interactive)
  (let ((file-name (file-relative-name buffer-file-name (vc-git-root buffer-file-name)))
        (repo-name (file-name-nondirectory (directory-file-name (vc-git-root buffer-file-name))))
        (git-url (magit-get "remote" "origin" "url"))
        (bitbucket-project-pattern ".*/\\([a-zA-Z0-9_-]+\\)/.*\\.git$")
        (bitbucket-revision (vc-working-revision (buffer-file-name) 'git))
        (bitbucket-project "FOO"))

    (when (string-match bitbucket-project-pattern git-url)
      (setq bitbucket-project (match-string 1 git-url)))

    (kill-new
     (concat "https://jira.stibodx.com/stash/projects/"
             bitbucket-project
             "/repos/" repo-name "/browse/" file-name
             "?at=" bitbucket-revision
             "#" (number-to-string (line-number-at-pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 30 White space
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tab-width 2)
(setq-default indent-tabs-mode nil)
(global-visual-line-mode)

;; This disables bidirectional text to prevent "trojan source"
;; exploits, see https://www.trojansource.codes/
(setf (default-value 'bidi-display-reordering) nil)

;; ws-butler cleans up whitespace only on the lines you've edited,
;; keeping messy colleagues happy ;-) Important that it doesn't clean
;; the whitespace on currrent line, otherwise, eclim leaves messy
;; code behind.
(use-package ws-butler
  :ensure t
  :init
  (setq ws-butler-keep-whitespace-before-point nil)
  :config
  (ws-butler-global-mode))

(defun tkj-indent-and-fix-whitespace()
  (interactive)
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))
(global-set-key "\C-\M-\\" 'tkj-indent-and-fix-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefer UTF 8, but don't override current encoding if specified
;; (unless you specify a write hook).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prefer-coding-system 'utf-8-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 40 Version control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing VC log messages
(add-hook 'log-edit-hook (lambda () (flyspell-mode 1)))

(use-package magit
  :ensure t
  :config
  (setq magit-log-arguments '("-n256" "--graph" "--decorate" "--color")
        ;; Show diffs per word, looks nicer!
        magit-diff-refine-hunk t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 50 Text buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ispell
  :init
  (setq ispell-program-name "hunspell"
        ispell-list-command "list"
        ;; The dictionary must reside in /usr/share/hunspell
        ispell-dictionary "en_GB")
  :hook ((prog-mode . flyspell-prog-mode)))

(defun tkj/copy-uri-encoded-region()
  "URI encodes the current active region and puts it on the kill
ring (clipboard)."
  (interactive)
  (kill-new
   (url-hexify-string
    (buffer-substring-no-properties (mark) (point)))))

(defun tkj/markdown-to-confluence ()
  "Convert all Markdown to Confluence/Jira markup in the selected
region or the entire buffer if no region is selected."
  (interactive)
  (save-excursion
    (let ((start (if (use-region-p) (region-beginning) (point-min)))
          (end (if (use-region-p) (region-end) (point-max))))

      (goto-char start) (while (re-search-forward "```.*" end t) (replace-match "{code}"))
      (goto-char start) (while (re-search-forward "`\\([^`]+\\)`" end t) (replace-match "{\\1}"))
      (goto-char start) (while (search-forward "######" end t) (replace-match "h6. "))
      (goto-char start) (while (search-forward "#####" end t) (replace-match "h5. "))
      (goto-char start) (while (search-forward "####" end t) (replace-match "h4. "))
      (goto-char start) (while (search-forward "###" end t) (replace-match "h3. "))
      (goto-char start) (while (search-forward "##" end t) (replace-match "h2. "))
      (goto-char start) (while (search-forward "#" end t) (replace-match "h1. "))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Latex and bibtex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq bibtex-align-at-equal-sign t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 55 Clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package xclip
  :ensure t
  :init
  (xclip-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Date, time and calendar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tkj/insert-date ()
  (interactive)
  (let (( time (current-time-string) ))
    (insert (format-time-string "%Y-%m-%d"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 60 Programming buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Highlight blocks of code in bold
(setq show-paren-style 'expression)
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; Don't ask before killing the current compilation. This is useful if
;; you're running servers after compiling them, so that the
;; compilation never finishes.
(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output t
      compile-command "~/bin/tc")

;; Convert shell escapes to color
(add-hook 'compilation-filter-hook
          (lambda () (ansi-color-apply-on-region (point-min) (point-max))))

;; Taken from
;; https://emacs.stackexchange.com/questions/31493/print-elapsed-time-in-compilation-buffer/56130#56130
(make-variable-buffer-local 'my-compilation-start-time)

(add-hook 'compilation-start-hook #'my-compilation-start-hook)
(defun my-compilation-start-hook (proc)
  (setq my-compilation-start-time (current-time)))

(add-hook 'compilation-finish-functions #'my-compilation-finish-function)
(defun my-compilation-finish-function (buf why)
  (let* ((elapsed  (time-subtract nil my-compilation-start-time))
         (msg (format "Compilation took: %s" (format-time-string "%T.%N" elapsed t))))
    (save-excursion (goto-char (point-max)) (insert msg))
    (message "Compilation %s: %s" (string-trim-right why) msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 65 Shells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-shell-mode-hook ()
  (process-send-string (get-buffer-process (current-buffer))
                       "export PAGER=cat\n")
  (process-send-string (get-buffer-process (current-buffer))
                       "uprompt\n\n\n"))
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 70 Projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  :bind (("C-t" . projectile-grep)
         ("C-c p g" . projectile-grep)))
(use-package counsel-projectile
  :ensure t
  :bind
  (("C-c p f" . counsel-projectile-find-file)
   ("C-c p p" . counsel-projectile-switch-project)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 71 fly check
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 71 snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :ensure t
  :config

  (setq yas/root-directory
        (list "~/src/skybert/my-little-friends/emacs/.emacs.d/snippets")
        yas-indent-line 'fixed)
  (yas-global-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 72 company - complete any aka company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :config
  (global-set-key (kbd "<C-return>") 'company-complete)
  (global-company-mode 1))

;; Get auto completion of :emoji: names.
(use-package company-emoji
  :ensure t
  :after company-mode
  :config
  (company-emoji-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 80 Java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-mode
  :ensure t
  :hook
  ((lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-enable-file-watchers nil
        lsp-headerline-breadcrumb-enable nil
        )

  ;; Performance tweaks, see
  ;; https://github.com/emacs-lsp/lsp-mode#performance
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)

  :bind
  (:map lsp-mode-map
        (("\C-\M-g" . lsp-find-implementation)
         ("M-RET" . lsp-execute-code-action)))
  )

(use-package hydra :ensure t)
(use-package lsp-ui :ensure t)
(use-package which-key :ensure t :config (which-key-mode))
(use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook 'lsp)
  (setq lsp-java-vmargs
        '(
          "-noverify"
          "-Xmx4G"
          "-XX:+UseG1GC"
          "-XX:+UseStringDeduplication"
          "-javaagent:/home/torstein/.m2/repository/org/projectlombok/lombok/1.18.32/lombok-1.18.32.jar"
          )

        lsp-java-java-path "/usr/lib/jvm/java-21-openjdk/bin/java"

        ;; Don't organise imports on save
        lsp-java-save-action-organize-imports nil

        ;; Don't format my source code (I use Maven for enforcing my
        ;; coding style)
        lsp-java-format-enabled nil))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package lsp-treemacs)

(defun tkj-default-code-style-hook()
  (setq c-basic-offset 2
        c-label-offset 0
        indent-tabs-mode nil
        compile-command "~/bin/tc"
        require-final-newline nil))
(add-hook 'java-mode-hook 'tkj-default-code-style-hook)

(defun my-java-mode-hook ()
  (auto-fill-mode)
  (flycheck-mode)
  (subword-mode)
  (yas-minor-mode)
  (when window-system
    (set-fringe-style '(8 . 0)))

  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))

  ;; Indent arguments on the next line as indented body.
  (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 81 JavaScript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq js-indent-level tab-width)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 82 BASH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sh-basic-offset tab-width
      sh-indentation tab-width)

;; snippets, please
(add-hook 'sh-mode-hook 'yas-minor-mode)

;; on the fly syntax checking
(add-hook 'sh-mode-hook 'flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 83 XML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq nxml-slash-auto-complete-flag t)

(defun tkj/tidy-up-xml()
  "Pretty print XML that's all on one line."
  (interactive)
  (goto-char 0)
  (replace-string "><" ">
<")
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "C-x t") 'tkj/tidy-up-xml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 84 YAML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 85 Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tkj/org-file-of-the-day()
  (interactive)
  (let ((daily-name (format-time-string "%Y/%Y-%m-%d")))
    (find-file
     (expand-file-name
      (concat "~/doc/scribbles/" daily-name ".org")))))

(defun tkj/org-update-agenda-files()
  (interactive)
  (setq org-agenda-files
        (list
         (concat "~/doc/scribbles/" (format-time-string "%Y") "/outlook.org")
         (concat "~/doc/scribbles/" (format-time-string "%Y") "/home.org")
         (concat "~/doc/scribbles/" (format-time-string "%Y") "/" (format-time-string "%Y-%m-%d") ".org")
         )))
(defun tkj/copy-jira-link()
  "Creates a Jira link out of the issue key at point. The function
then inserts it into your kill ring so you can paste/yank it
where you need it."
  (interactive)
  (let ((issue (thing-at-point 'filename 'no-properties)))
    (kill-new (concat "https://jira.stibodx.com/browse/" issue))))

;; Open Jira issue references in the browser
(setq bug-reference-bug-regexp "\\b\\(\\([A-Za-z][A-Za-z0-9]\\{1,10\\}-[0-9]+\\)\\)"
      bug-reference-url-format "https://jira.stibodx.com/browse/%s")
(add-hook 'org-mode-hook 'bug-reference-mode)

;; Pretty Org tables
;; (progn
;;   (add-to-list 'load-path "/usr/local/src/org-pretty-table")
;;   (require 'org-pretty-table)
;;   (add-hook 'org-mode-hook (lambda () (org-pretty-table-mode))))

(use-package org
  :init
  (setq org-clock-mode-line-total 'today
        org-fontify-quote-and-verse-blocks t
        org-indent-mode t
        org-return-follows-link t
        org-startup-folded 'content
        org-todo-keywords '((sequence "🆕(t)" "▶️(s)" "⏳(w)" "🔎(p)" "|" "✅(d)" "🗑(c)" "👨(g)"))
        )

  :config
  (add-hook 'org-mode-hook 'org-indent-mode)

  :bind
  (("\C-ca" . org-agenda)
   ("\C-ct" . org-capture)
   ("\C-cl" . tkj/org-file-of-the-day))
  ("\C-cu" . tkj/org-update-agenda-files))

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list '("❯" "❯❯" "❯❯❯" "❯❯❯❯" "❯❯❯❯❯"))
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 86 HTML and CSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq css-indent-offset tab-width)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 87 Perl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq perl-indent-level tab-width)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 88 Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("Pipfile" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.kv\\'" . yaml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 90 AI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tkj/load-ai()
  (interactive)

  (add-to-list 'load-path "/usr/local/src/c3po.el")
  (add-to-list 'load-path "/usr/local/src/copilot.el")
  (require 'c3po)

  (use-package editorconfig)
  (require 'copilot)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 95 envrc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package envrc
  :ensure t
  :init
  (envrc-global-mode))
