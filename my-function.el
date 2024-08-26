(defun my-split-window-below (&optional arg)
  "Split the current window 70/30 rather than 50/50.
A single-digit prefix argument gives the top window arg*10%."
  (interactive "P")
  (let ((proportion (* (or arg 7) 0.1)))
    (split-window-below (round (* proportion (window-height))))))

;; font
;(defvar my-font-list '("Andika" "Helvetica" "JetBrains Mono" "IntelOne Mono" "Roboto Mono" "Fira Code" "Lucida Console")
;    "List of fonts to cycle through.")

;; get all font families
;(defvar my-font-list (font-family-list))

(cond
 ((eq system-type 'windows-nt)
  (defvar my-font-list '("Andika" "Helvetica" "JetBrains Mono" "IntelOne Mono" "Roboto Mono" "Fira Code" "Lucida Console")
    "List of fonts to cycle through."))
 ((eq system-type 'gnu/linux)
  (defvar my-font-list (font-family-list))))

(defvar my-current-font-index 0
    "Index of the current font in the font list.")

(defun my-set-next-font ()
    "Set the next font from the font list."
    (interactive)
    (setq my-current-font-index (mod (1+ my-current-font-index) (length my-font-list)))
    (set-frame-font (nth my-current-font-index my-font-list))
    (message "current font: %s" (nth my-current-font-index my-font-list)))

;; theme
(defvar my-theme-list
    '(deeper-blue
        tsdh-dark
        manoj-dark))

(defvar my-current-theme-index 0
    "Index of the current theme in the font list.")

(defun my-next-theme ()
    "Set the next theme from the theme list."
    (interactive)
    (setq my-current-theme-index (mod (1+ my-current-theme-index) (length my-theme-list)))
    (load-theme (nth my-current-theme-index my-theme-list))
    (message "current theme: %s" (car custom-enabled-themes)))


(defun my-get-all-themes ()
    "Get a list of all available themes."
    (interactive)
    (message "Available themes: %s" (custom-available-themes)))

(setq my-theme-list (custom-available-themes))

(defun maximize-frame ()
    "Maximize the current frame."
    (interactive)
    (set-frame-parameter nil 'fullscreen 'maximized))

(defun minimize-frame ()
    "Minimize the current frame."
    (interactive)
    (set-frame-parameter nil 'fullscreen 'minimized))

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

(defun replace-file (source-file target-file)
    "Replace the contents of TARGET-FILE with the contents of SOURCE-FILE."
    (interactive "fSource file: \nfTarget file: ")
    (with-temp-buffer
        (insert-file-contents source-file)
        (write-region (point-min) (point-max) target-file)))    

(defun launch-separate-emacs-in-terminal ()
  (suspend-emacs "fg ; emacs -nw"))

(defun launch-separate-emacs-under-x ()
  ;(call-process "sh" nil nil nil "-c" "emacs &"))
  (call-process "runemacs"))

(defun restart-emacs ()
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
                                                           #'launch-separate-emacs-under-x
                                                         #'launch-separate-emacs-in-terminal)))))
(save-buffers-kill-emacs)))        

;; ========================================
;; 開啟以當天日期為名的檔案 ex: 2024-08-22.org
;; ========================================
;(find-file "d:/Ray/MyProjects/gitweb/MyNotes/Daily")

;(defun open-today-org-file ()
;  "Open an .org file named with today's date in the format YYYY-MM-DD."
;  (let ((filename (format-time-string "%Y-%m-%d.org")))
;    (find-file (expand-file-name filename "~/path/to/your/directory"))))

;(add-hook 'emacs-startup-hook 'open-today-org-file)

;(open-today-org-file)

;; ==========================================
;; 開啟以前一日日期為名的檔案 ex: 2024-08-22.org
;; ==========================================

(defun get-previous-day ()
  "Return the previous day's date as a string in the format YYYY-MM-DD."
  (let* ((current-time (current-time))
         (previous-day (time-subtract current-time (days-to-time 1))))
    (format-time-string "%Y-%m-%d" previous-day)))

;; 測試函數
;(message (get-previous-day))

(let ((filename (concat (get-previous-day) ".org")))
  (find-file (expand-file-name filename "~/path/to/your/directory")))


;; ====================================================================
;; 檢查以當天日期命名的 .org 文件是否存在。
;; 如果存在，則打開該文件；如果不存在，則檢查並打開以前一天日期命名的 .org 文件。
;; 如果兩個文件都不存在，則顯示一條消息。
;; ====================================================================
(defun open-dated-org-file ()
  "Open today's .org file if it exists, otherwise open yesterday's .org file."
  (let* ((today (format-time-string "%Y-%m-%d.org"))
         (yesterday (format-time-string "%Y-%m-%d.org" (time-subtract (current-time) (days-to-time 1))))
         (today-path (expand-file-name today "~/path/to/your/directory"))
         (yesterday-path (expand-file-name yesterday "~/path/to/your/directory")))
    (if (file-exists-p today-path)
        (find-file today-path)
      (if (file-exists-p yesterday-path)
          (find-file yesterday-path)
        (message "Neither today's nor yesterday's .org file exists.")))))

(add-hook 'emacs-startup-hook 'open-dated-org-file)

;; ====================================================================
;; backup-my-emacs-init 功能:
;; 能夠將 init.el 複製一份並以當天日期時間為檔案名,例如init-20240101-123456.el
;; ====================================================================
(defun backup-my-emacs-init ()
  "Backup the current init.el file with a timestamp."
  (interactive)
  (let* ((init-file (expand-file-name "init.el" user-emacs-directory))
         (backup-file (concat user-emacs-directory
                              "init-"
                              (format-time-string "%Y%m%d-%H%M%S")
                              ".el")))
    (if (file-exists-p init-file)
        (copy-file init-file backup-file t)
      (message "init.el does not exist!"))))

;; Bind the function to a key for easy access, e.g., F5
(global-set-key (kbd "<f5>") 'backup-my-emacs-init)

;; ====================================================================
;; 改成備份 ~/.emacs檔案
;; ====================================================================
(defun backup-my-emacs-file ()
  "Backup the current ~/.emacs file with a timestamp."
  (interactive)
  (let* ((emacs-file (expand-file-name "~/.emacs"))
         (backup-file (concat (file-name-directory emacs-file)
                              "emacs-"
                              (format-time-string "%Y%m%d-%H%M%S")
                              ".el")))
    (if (file-exists-p emacs-file)
        (copy-file emacs-file backup-file t)
      (message "~/.emacs does not exist!"))))

;; Bind the function to a key for easy access, e.g., F5
(global-set-key (kbd "<f5>") 'backup-my-emacs-file)

;; ====================================================================
;; 備份 ~/.emacs檔案,加上如果備份成功, 顯示 "備份成功!" 的英文
;; ====================================================================
(defun backup-my-emacs-file ()
  "Backup the current ~/.emacs file with a timestamp."
  (interactive)
  (let* ((emacs-file (expand-file-name "~/.emacs"))
         (backup-file (concat (file-name-directory emacs-file)
                              "emacs-"
                              (format-time-string "%Y%m%d-%H%M%S")
                              ".el")))
    (if (file-exists-p emacs-file)
        (progn
          (copy-file emacs-file backup-file t)
          (message "Backup successful!"))
      (message "~/.emacs does not exist!"))))

;; Bind the function to a key for easy access, e.g., F5
(global-set-key (kbd "<f5>") 'backup-my-emacs-file)

;; ====================================================================
;; 備份 ~/.emacs 及 ~/.emacs.d/init.el 到 ~/.emacs.d/backup
;; ====================================================================
(defun backup-emacs-config ()
  "Backup ~/.emacs and ~/.emacs.d/init.el to ~/.emacs.d/backup with date suffix."
  (interactive)
  (let* ((backup-dir "~/.emacs.d/backup/")
         (date-suffix (format-time-string "%Y-%m-%d"))
         (emacs-file "~/.emacs")
         (init-file "~/.emacs.d/init.el")
         (emacs-backup-file (concat backup-dir ".emacs-" date-suffix))
         (init-backup-file (concat backup-dir "init.el-" date-suffix)))
    ;; Create backup directory if it doesn't exist
    (unless (file-directory-p backup-dir)
      (make-directory backup-dir t))
    ;; Copy ~/.emacs if it exists
    (when (file-exists-p emacs-file)
      (copy-file emacs-file emacs-backup-file t))
    ;; Copy ~/.emacs.d/init.el if it exists
    (when (file-exists-p init-file)
      (copy-file init-file init-backup-file t))
    (message "Backup completed: %s and %s" emacs-backup-file init-backup-file)))

;; Bind the function to a key for easy access, e.g., F5
(global-set-key (kbd "<f5>") 'backup-emacs-config)
