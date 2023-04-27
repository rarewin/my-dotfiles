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

;; パッケージのインストール
(straight-use-package 'company-mode)
(straight-use-package 'counsel)
(straight-use-package 'ddskk)
(straight-use-package 'elscreen)
(straight-use-package 'kotlin-mode)
(straight-use-package 'lsp-mode)
(straight-use-package 'moe-theme)
(straight-use-package 'powerline)
(straight-use-package 'rustic)
(straight-use-package 'swift-mode)
(straight-use-package 'yasnippet)


;; バックアプファイルを無効
(setq make-backup-files nil)
(setq auto-save-default nil)

;; キーバインド
(global-set-key (kbd "<f5>") 'revert-buffer)   ; f5 - revert buffer
(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)        ; f11 - full screen
(global-set-key "\C-h" 'backward-delete-char)  ; C-h should be backspace!
(global-set-key "\C-j" nil)                    ; C-j
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key "\C-x\C-f" 'counsel-find-file)


;; moe-theme & powerline
(require 'powerline)
(require 'moe-theme)
(moe-dark)
(powerline-moe-theme)
(setq moe-theme-select-color 'cyan)
(set-face-foreground 'font-lock-comment-face "pink")
;(set-face-foreground 'whitespace-tab "darkblue")
;(set-face-underline  'whitespace-tab t)
;(set-face-background 'whitespace-tab nil)

;; 表示設定
(menu-bar-mode 0)
(tool-bar-mode 0)
;(scroll-bar-mode 0)


;; IME
(setq default-input-method "japanese-skk")

;; elscreen
(global-unset-key (kbd "C-z"))
(load "elscreen")
(setq elscreen-prefix-key (kbd "C-z"))
(elscreen-start)

;; rustic
(setq rustic-format-trigger 'on-save)


;; start emacs-server
(server-start)
