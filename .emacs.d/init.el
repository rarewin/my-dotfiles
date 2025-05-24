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

;; org-modeの設定
(use-package org
  :if (not (eq system-type 'windows-nt))
  :custom
  (org-agenda-files (list "~/Org"
                          "~/Org/journal"))
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-todo-keywords
   '((sequence "TODO(t)" "SOMEDAY(s)" "PENDING(p)" "WAITING(w)" "|" "DONE(d)" "GIVEUP(u)")))
  )

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(use-package org-journal
  :after org
  :defer t
  :custom
  (org-journal-dir "~/Org/journal")
  (org-journal-date-format "%Y-%m-%d %A")
  (org-journal-time-format "%m/%d %R")
  (org-journal-carryover-items "TODO=\"TODO\"|TODO=\"PENDING\"|TODO=\"SOMEDAY\"|TODO=\"WAITING\"")
  (org-journal-file-format "%Y/%m/%Y%m%d.org")
  :bind
  ("C-c C-j" . org-journal-new-entry)
  )

(use-package org-roam
  :after org
  :custom
  (org-roam-directory (file-truename "~/Org/roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  ; (require 'org-roam-protocol)
  )

(use-package elscreen
  :config
  (setq elscreen-prefix-key (kbd "C-z"))
  :init
  (elscreen-start)
  )

(use-package counsel)
(use-package ddskk
  :commands skk-mode
  :init
  (setq default-input-method "japanese-skk")
  :config
  (prefer-coding-system 'utf-8)
  (require 'skk-autoloads)
  (setq skk-server-host "localhost")
  (setq skk-server-port 1178)
  (setq skk-search-prog-list
	'((skk-search-server skk-server-host skk-server-port)
	  (skk-search-jisyo-file skk-jisyo 0)))
  )

(use-package powerline)
(use-package moe-theme
  :config
  (moe-dark)
  (powerline-moe-theme)
  (set-face-foreground 'font-lock-comment-face "pink")
  :custom
  (moe-theme-select-color 'cyan)
  )


;; バックアプファイルを無効
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 表示設定
(menu-bar-mode 0)
(tool-bar-mode 0)

;; GUIのときだけスクロールモードをoff設定に
(when (display-graphic-p)
  (scroll-bar-mode -1))

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
