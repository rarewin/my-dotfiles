;; Package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)


;; global key settings
(global-set-key (kbd "<f5>") 'revert-buffer)   ; f5 - revert buffer
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
(scroll-bar-mode 0)

;; format for C programs
(defun my-c-mode-common-hook ()
  (c-set-style "linux") (setq indent-tabs-mode t) ;linux style
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; GFM (GitHub Flavored Markdown)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; web-mode
(setq web-mode-engines-alist
      '(("django" . "\\.tpl.html\\'"))       ;; jinja2
      )

(defun my-web-mode-hook ()
  "Hook for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq indent-tabs-mode nil)
  )
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; python-mode
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

;; $B%?%VI}(B.
(setq-default tab-width 8)

;; $B9TKv$N@^$jJV$7(B.
(setq-default truncate-partial-width-windows t)
(setq-default truncate-lines t)

;; $B%O!<%I%?%V$r;H$&(B.
(setq-default indent-tabs-mode t)

;; high light trailing white space
(setq-default show-trailing-whitespace t)

;; $B9THV9fI=<((B.
(require 'linum)
(global-linum-mode)

;; $B3g8L$rI=<((B.
(show-paren-mode 1)

;; $B8=:_9T$N%O%$%i%$%H(B.
(global-hl-line-mode t)

;; $BA*BrHO0O$N%O%$%i%$%H(B.
(transient-mark-mode t)

;; $B%?%VI}JQ994X?t(B
(defun set-aurora-tab-width (num &optional local redraw)
  "$B%?%VI}$r%;%C%H$7$^$9!#%?%V(B5$B$H$+%?%V(B20$B$b@_Dj$G$-$?$j$7$^$9!#(B
local$B$,(B non-nil$B$N>l9g$O!"%+%l%s%H%P%C%U%!$G$N$_M-8z$K$J$j$^$9!#(B
redraw$B$,(B non-nil$B$N>l9g$O!"(BWindow$B$r:FIA2h$7$^$9!#(B"
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
		    (set-frame-parameter nil 'alpha 90) ;$BF)L@EY(B
		    ))

;; Fonts
(let* ((size 10)
       (asciifont "Ricty Diminished Discord")
       (jpfont "Ricty Diminished Discord")
       (h (* size 10))
       (fontspec (font-spec :family asciifont))
       (jp-fontspec (font-spec :family jpfont)))
  (set-face-attribute 'default nil :family asciifont :height h)
  (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
  (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
  (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
  (set-fontset-font nil '(#x0080 . #x024F) fontspec)
  (set-fontset-font nil '(#x0370 . #x03FF) fontspec))

;; theme
(require 'moe-theme)
(moe-dark)

;; org-babel
;; iPython
(require 'ob-ipython)
(require 'ob-ruby)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   (irb . t)
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
    (company-inf-ruby inf-ruby migemo go-mode kotlin-mode ob-go ob-kotlin ob-swift rust-mode swift-mode swift3-mode ob-ipython moe-theme counsel twittering-mode mhc smex yatex auto-complete ein wanderlust ddskk elscreen org)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
