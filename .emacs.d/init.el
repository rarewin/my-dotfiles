;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; パッケージのインストール
(straight-use-package 'use-package)

(straight-use-package 'company-mode)
(straight-use-package 'counsel)
(straight-use-package 'ddskk)
(straight-use-package 'eglot)
(straight-use-package 'elscreen)
(straight-use-package 'kotlin-mode)
(straight-use-package 'lsp-mode)
(straight-use-package 'moe-theme)
(straight-use-package 'org-mode)  ;; makeがない……
(straight-use-package 'org-journal)
(straight-use-package 'org-roam)
(straight-use-package 'org-roam-ui)
(straight-use-package 'powerline)
(straight-use-package 'python-black)
(straight-use-package 'python-mode)
(straight-use-package 'reformatter)
(straight-use-package 'review)
(straight-use-package 'rustic)
(straight-use-package 'swift-mode)
(straight-use-package 'web-mode)
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

;; yasnippet
(yas-global-mode)

;; 行番号表示.
(require 'linum)
(global-linum-mode)

;; 括弧を表示.
(show-paren-mode 1)

;; 現在行のハイライト.
(global-hl-line-mode t)

;; 選択範囲のハイライト.
(transient-mark-mode t)

;; 折り返さない
(set-default 'truncate-lines t)

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

;; 無駄な文字のハイライト
(setq-default show-trailing-whitespace t)
;(setq-default whitespace-space-regexp "\x3000+")
(setq-default whitespace-style
	      '(face
		tabs
		;; spaces
		trailing
		;; lines
		space-before-tab
		newline
		;; indentation
		empty
		space-after-tab
		;; big-indent
		;; space-mark
		;; tab-mark
		))
(setq-default whitespace-line-column 160)
(global-whitespace-mode 1)

;; make C-a lovely
(define-key global-map "\C-a"
#'(lambda (arg)
      (interactive "p")
      (if (looking-at "^")
	  (back-to-indentation)
	(beginning-of-line arg))))

; Org-captureの設定
; Org-captureを呼び出すキーシーケンス
(define-key global-map "\C-cc" 'org-capture)
; Org-captureのテンプレート（メニュー）の設定
(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/Org/notes.org" "Notes")
         "* %?\nEntered on %U\n %i\n %a")
        ))

(defun show-org-buffer (file)
  "Show an org-file FILE on the current buffer."
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (concat "~/Org/" file))))
(global-set-key (kbd "C-M-^") '(lambda () (interactive)
                                 (show-org-buffer "notes.org")))

(setq org-agenda-files (list "~/Org"
                             "~/Org/journal"))
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "SOMEDAY(s)" "PENDING(p)" "WAITING(w)" "|" "DONE(d)" "GIVEUP(u)")))

(setq org-journal-dir "~/Org/journal")
(setq org-journal-date-format "%Y %m/%d %A")
(setq org-journal-time-format "%m/%d %R")

(setq org-journal-carryover-items
      "TODO=\"TODO\"|TODO=\"PENDING\"|TODO=\"SOMEDAY\"|TODO=\"WAITING\"")
(setq org-journal-file-format "%Y%m%d.org")

;; rustic
(setq rustic-format-trigger 'on-save)

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))

;; biome <https://zenn.dev/craneduck/articles/3c0da51d4767d5>
(reformatter-define biome-format ;; 第1引数はわかりやすい自由な名前
  :program "npx" ;; コマンド。PATHが通っていること(後述のadd-node-modules-pathを使っても良い)
  :args `("biome" "format" "--stdin-file-path" ,(buffer-file-name)) ;; オプション引数のリスト、実行時に評価される
  :lighter " BiomeFmt") ;; minor-modeとして使う場合の、モードラインに表示する名前(先頭にスペースが必要)

(reformatter-define biome-check
  :program "npx"
  :args `("biome" "check" "--apply-unsafe" "--stdin-file-path" ,(buffer-file-name))
  :lighter " BiomeCheck")

;; Cでのインデントをいじる
(add-hook 'c-mode-hook
  '(lambda ()
     (c-set-offset 'extern-lang-open 0)
     (c-set-offset 'extern-lang-close 0)
     (c-set-offset 'inextern-lang 0))
  )

;; web-mode
(add-to-list 'auto-mode-alist
	     '("\\.ts[x]?\\'" . web-mode)
	     )
;(add-hook 'web-mode-hook 'lsp)


;; Windowsとのコピペ処理
(defun wsl-copy (start end)
  (interactive "r")
  (shell-command-on-region start end "iconv -t utf16 | clip.exe")
  (deactivate-mark)
  )

(defun wsl-paste ()
  (interactive)
  (let ((clipboard
     (shell-command-to-string "powershell.exe -command 'Get-Clipboard' | nkf -u")))
    (setq clipboard (replace-regexp-in-string "\r" "" clipboard)) ; Remove Windows ^M characters
    (setq clipboard (substring clipboard 0 -1)) ; Remove newline added by Powershell
    (insert clipboard)))


;; org-roam
(setq org-roam-v2-ack t)
(setq org-roam-db-location "~/Org/org-roam.db")
(setq org-roam-directory "~/Org/org-roam/")
; (setq org-roam-graph-executable "/opt/homebrew/bin/dot")
(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n g") 'org-roam-graph)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)
(require 'org-roam)
(org-roam-setup)

;; start emacs-server
(server-start)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "outline" :slant normal :weight normal :height 90 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval add-hook 'before-save-hook #'biome-check-buffer nil t))))
