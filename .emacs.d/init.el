;; Package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(global-set-key (kbd "<f5>") 'revert-buffer)          ;; f5 - revert buffer

(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|127\\.0\\..*\\|baobab\\)")))

;; no backup by Emacs
(setq make-backup-files nil)
(setq auto-save-default nil)

;; C-h should be backspace!
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-j" nil)

;; C-aを素敵挙動に.
(define-key global-map "\C-a"
  #'(lambda (arg)
      (interactive "p")
      (if (looking-at "^")
	  (back-to-indentation)
	(beginning-of-line arg))))

;; C プログラムの書式
(defun my-c-mode-common-hook ()
  (c-set-style "linux") (setq indent-tabs-mode t) ;linux 式がいいとき
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Markdown command (only for windows)
(if (eq system-type 'windows-nt)
    (setq markdown-command "\"c:\\Program Files\\nodejs\\node\" ${USERPROFILE}/AppData/Roaming/npm/node_modules/marked/bin/marked")
  )

;; style sheet for Markdown
(setq markdown-css-paths '("http://bootswatch.com/cerulean/bootstrap.css"))

;; GFM (GitHub Flavored Markdown)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; web-mode
(setq web-mode-engines-alist
      '(("django" . "\\.tpl.html\\'"))       ;; jinja2
      )

;; web-mode設定.
(defun my-web-mode-hook ()
  "Hook for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq indent-tabs-mode nil)
  )
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; python-mode設定.
(defun my-python-mode-hook ()
  "Hook for Python mode."
  (setq python-indent-guess-indent-offset 4)
  (setq indent-tabs-mode nil)
  )
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; IME
(setq default-input-method "japanese-skk")

;; elscreen
(load "elscreen")
(setq elscreen-prefix-key (kbd "C-z"))
(elscreen-start)

;; Twittering-mode
;; (add-to-list 'load-path "~/local/elisp/twittering-mode/")
;; (require 'twittering-mode)
;; (setq twittering-use-master-password t)

;; タブ幅.
(setq-default tab-width 8)

;; 行末の折り返し.
(setq-default truncate-partial-width-windows t)
(setq-default truncate-lines t)

;; ハードタブを使う.
(setq-default indent-tabs-mode t)

;; 行番号表示.
(require 'linum)
(global-linum-mode)

;; 括弧を表示.
(show-paren-mode 1)

;; 現在行のハイライト.
(global-hl-line-mode t)

;; 選択範囲のハイライト.
(transient-mark-mode t)

;; タブ幅変更関数
(defun set-aurora-tab-width (num &optional local redraw)
  "タブ幅をセットします。タブ5とかタブ20も設定できたりします。
localが non-nilの場合は、カレントバッファでのみ有効になります。
redrawが non-nilの場合は、Windowを再描画します。"
  (interactive "nTab Width: ")
  (when local
    (make-local-variable 'tab-width)
    (make-local-variable 'tab-stop-list))
  (setq tab-width num)
  (setq c-basic-offset num)
  (setq tab-stop-list ())
  (while (<= num 256)
    (setq tab-stop-list `(,@tab-stop-list ,num))
    (setq num (+ num tab-width)))
  (when redraw (redraw-display)) tab-width)

(define-key global-map "\C-c\C-t" 'set-aurora-tab-width)

;; transparent window
(if window-system (progn
		    (set-frame-parameter nil 'alpha 90) ;透明度
		    ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango-dark)))
 '(package-selected-packages (quote (ein wanderlust ddskk elscreen org)))
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ricty Diminished Discord" :foundry "outline" :slant normal :weight normal :height 98 :width normal)))))

;; emacs-server
(server-start)
