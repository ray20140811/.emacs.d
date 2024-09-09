;; 初始化包管理器
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; 安裝 use-package 來管理插件
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; 基本設置
(setq inhibit-startup-message t)  ;; 關閉啟動畫面
(tool-bar-mode -1)                ;; 關閉工具欄
(menu-bar-mode -1)                ;; 關閉菜單欄
(scroll-bar-mode -1)              ;; 關閉滾動條
(global-linum-mode t)             ;; 顯示行號

;; 安裝和配置常用插件
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :config
  (counsel-mode 1))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; 自定義快捷鍵
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-x") 'counsel-M-x)

;; 加載自定義文件
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "custom-file.el" t)

(provide 'copilot)
;;; init.el ends here
