;
; Package
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; company
(require 'company-lsp)
(push 'company-lsp company-backends)
;(global-company-mode +1)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)

;; company-tern (for Java Script)
(add-to-list 'company-backends 'company-tern)

;; company-jedi (for Python)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; C/C++
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'objc-mode-hook 'lsp)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(require 'clang-format)
;(add-hook 'c-mode-common-hook
;          (function (lambda ()
;                    (add-hook 'before-save-hook
;                              'clang-format-buffer))))
;(add-hook 'c++-mode-common-hook
;          (function (lambda ()
;                    (add-hook 'before-save-hook
;                              'clang-format-buffer))))
;(add-hook 'objc-mode-common-hook
;          (function (lambda ()
;                    (add-hook 'before-save-hook
;                              'clang-format-buffer))))

;; Rust
;(add-hook 'rust-mode-hook (lambda ()
;                            (racer-mode)
;                            (flycheck-rust-setup)))
(eval-after-load "rust-mode"
  '(setq-default rust-format-on-save t))
(add-hook 'rust-mode-hook 'lsp)

;; git-gutter+
(global-git-gutter+-mode)

;; global key settings
(global-set-key (kbd "<f5>") 'revert-buffer)   ; f5 - revert buffer
(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)	; f11 - full screen
(global-set-key "\C-h" 'backward-delete-char)  ; C-h should be backspace!
(global-set-key "\C-j" nil)                    ; C-j
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key "\C-x\C-f" 'counsel-find-file)


;; make C-a lovely
(define-key global-map "\C-a"
#'(lambda (arg)
      (interactive "p")
      (if (looking-at "^")
	  (back-to-indentation)
	(beginning-of-line arg))))

;; no backup by Emacs
(setq make-backup-files nil)
(setq auto-save-default nil)

;; hide menu-bar, tool-bar, and scroll-bar
(menu-bar-mode 0)
(tool-bar-mode 0)
;; (scroll-bar-mode 0)

;; format for C programs
(defun my-c-mode-common-hook ()
  (c-set-style "linux") (setq indent-tabs-mode t) ;linux style
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; GFM (GitHub Flavored Markdown)
(add-to-list 'auto-mode-alist '("\\.md" . gfm-mode))

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?" . web-mode))
(setq web-mode-engines-alist
      '(("django" . "\\.tpl.html")       ;; jinja2
	("vue" . "\\.vue")
	("jsx" . "\\.jsx?")              ;; React
	))

(defun my-web-mode-hook ()
  "Hook for Web mode."
  (setq web-mode-attr-indent-offset nil)
  (setq web-mode-attr-value-indent-offset nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-sql-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  )
(add-hook 'web-mode-hook 'my-web-mode-hook)

(setq web-mode-enable-engine-detection t)

;; python-mode
; (defun my-python-mode-hook ()
;   "Hook for Python mode."
;   (setq python-indent-guess-indent-offset 4)
;   (setq indent-tabs-mode nil)
;   )
; (add-hook 'python-mode-hook 'my-python-mode-hook)

;; elpy
(use-package elpy
  :ensure t
  :init
  (elpy-enable))
(defun my-elpy-mode-hook ()
  "Hook for elpy mode"
  (add-hook 'before-save-hook 'elpy-format-code)
  )
(add-hook 'elpy-mode-hook 'my-elpy-mode-hook)
(setq elpy-rpc-python-command "python3")

;; kotlin-mode
(defun my-kotolin-mode-hook ()
  "Hook for Kotlin mode."
  (setq kotlin-tab-width 4)
  (setq indent-tabs-mode nil)
  )
(add-hook 'kotlin-mode-hook 'my-kotolin-mode-hook)

;; YaTeX-mode
(load "yatex")
(add-to-list 'auto-mode-alist '("\\.tex" . yatex-mode))

;; golang
(add-hook 'before-save-hook #'gofmt-before-save)


;; IME
(setq default-input-method "japanese-skk")

;; elscreen
(global-unset-key (kbd "C-z"))
(load "elscreen")
(setq elscreen-prefix-key (kbd "C-z"))
(elscreen-start)

;; xclip
(xclip-mode 1)

;; タブ幅.
(setq-default tab-width 8)

;; 行末の折り返し.
(setq-default truncate-partial-width-windows t)
(setq-default truncate-lines t)

;; ハードタブを使う.
(setq-default indent-tabs-mode t)

;; high light trailing white space
(setq-default show-trailing-whitespace t)

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

;; Fonts
;(let* ((size 10)
;       (asciifont "Ricty Diminished Discord")
;       (jpfont "Ricty Diminished Discord")
;       (h (* size 10))
;       (fontspec (font-spec :family asciifont))
;       (jp-fontspec (font-spec :family jpfont)))
;  (set-face-attribute 'default nil :family asciifont :height h)
;  (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
;  (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
;  (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
;  (set-fontset-font nil '(#x0080 . #x024F) fontspec)
;  (set-fontset-font nil '(#x0370 . #x03FF) fontspec))

;; theme
(require 'powerline)
(require 'moe-theme)
(moe-dark)
(powerline-moe-theme)
(moe-theme-set-color 'cyan)
(set-face-foreground 'font-lock-comment-face "pink")


;; for org-mode
(require 'ox-asciidoc)
(require 'ox-gfm)
(require 'ox-rst)
(require 'ox-textile)

(require 'ob-ipython)
(require 'ob-ruby)
(require 'ob-go)
(require 'ob-kotlin)
(require 'ob-swift)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   (go . t)
   (kotlin . t)
   (swift . t)
   (ruby . t)
   ))

;; start emacs-server
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(package-selected-packages
   (quote
    (ecb web-mode-edit-element web-mode company-racer flycheck flycheck-clangcheck flycheck-kotlin flycheck-perl6 flycheck-rust kotlin-mode ox-asciidoc ox-gfm ox-rst ox-textile cargo company-inf-ruby inf-ruby migemo go-mode ob-go ob-kotlin ob-swift rust-mode swift-mode swift3-mode ob-ipython moe-theme counsel twittering-mode mhc smex yatex auto-complete ein wanderlust ddskk elscreen org)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
