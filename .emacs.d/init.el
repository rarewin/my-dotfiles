;; キーバインド
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<f5>") 'revert-buffer)   ; f5 - revert buffer
(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)        ; f11 - full screen
(global-set-key "\C-h" 'backward-delete-char)  ; C-h should be backspace!
(global-set-key "\C-j" nil)                    ; C-j
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key "\C-x\C-f" 'counsel-find-file)

;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package eglot)
(add-to-list 'eglot-server-programs
	     '(python-mode . ("pyright-langserver" "--stdio")))
(use-package python-mode
  :ensure nil
  :hook
  (python-mode . eglot-ensure))

(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-backends '(company-capf))
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last))
  )

(use-package elscreen
  :config
  (setq elscreen-prefix-key (kbd "C-z"))
  :init
  (elscreen-start)
  )

(use-package counsel)

(use-package powerline)
(use-package moe-theme
  :config
  (moe-dark)
  (powerline-moe-theme)
  (setq moe-theme-select-color 'cyan)
  (set-face-foreground 'font-lock-comment-face "pink")
  )



;; バックアプファイルを無効
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 表示設定
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; 括弧を表示.
(show-paren-mode 1)

;; 現在行のハイライト.
(global-hl-line-mode t)

;; 選択範囲のハイライト.
(transient-mark-mode t)

;; 折り返さない
(setq truncate-lines t)

;; make C-a lovely
(define-key global-map "\C-a"
#'(lambda (arg)
      (interactive "p")
      (if (looking-at "^")
	  (back-to-indentation)
	(beginning-of-line arg))))


(global-display-line-numbers-mode 1)
