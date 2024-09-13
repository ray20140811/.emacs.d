;; Demo
(progn
  (vertico-mode 'toggle)
  (marginalia-mode 'toggle))

(vertico-mode 1)
(vertico-mode -1)

(marginalia-mode 1)

;; 安裝和配置 Vertico
(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1))

;; 安裝和配置 Marginalia
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

;; 安裝和配置 Consult
(use-package consult
  :ensure t
  :bind
  (("C-s" . consult-line) ;; 綁定 Consult 的搜索命令到 C-s
   ("M-y" . consult-yank-pop) ;; 綁定 Consult 的黏貼命令到 M-y
   ("C-x b" . consult-buffer) ;; 綁定 Consult 的切換buffer到 C-x b
    ;;
   ))
  
