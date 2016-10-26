;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     yaml
     windows-scripts
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     ;; better-defaults
     emacs-lisp
     git
     markdown
     org
     csharp
     gtags
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     syntax-checking
     ;; version-control
     perspectives
     c-c++
     java
     python
     ruby
     rust
     html
     javascript
     go
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Ricty Diminished"
			       :weight bold)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."

  (setq url-proxy-services
	'(("no_proxy" . "^\\(localhost\\|127\\.0\\..*\\)")))

  ;; C-hはバックスペース.
  (global-set-key "\C-h" 'backward-delete-char)
  (global-set-key "\C-j" nil)

  ;; C-aを素敵挙動に.
  (define-key global-map "\C-a"
    #'(lambda (arg)
        (interactive "p")
        (if (looking-at "^")
            (back-to-indentation)
          (beginning-of-line arg))))

  ;; ESCはMetaキー相当でいいんだよ!!
  (add-hook 'holy-mode-hook (lambda () (define-key evil-emacs-state-map [escape] nil)))

  ;; C プログラムの書式
  (defun my-c-mode-common-hook ()
    (c-set-style "linux") (setq indent-tabs-mode t) ;linux 式がいいとき
    )
  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

  ;; skk
  ;(add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
  ;(add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)

  (setq dotspacemacs-elpa-https nil)

  ;; Markdown command (only for windows)
  (if (eq system-type 'windows-nt)
      (setq markdown-command "\"c:\\Program Files\\nodejs\\node\" C:/Users/katsu/AppData/Roaming/npm/node_modules/marked/bin/marked")
    )

  ;; style sheet for Markdown
  (setq markdown-css-paths '("http://bootswatch.com/cerulean/bootstrap.css"))

  ;; GFM (GitHub Flavored Markdown)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

  ;; web-mode
  (setq web-mode-engines-alist
	'(("django" . "\\.tpl.html\\'"))       ;; jinja2
	)


  ;; windowの大きさを変更する.
  ;; <http://d.hatena.ne.jp/mooz/20100119/p1>
  (defun window-resizer ()
    "Control window size and position."
    (interactive)
    (let ((window-obj (selected-window))
	  (current-width (window-width))
	  (current-height (window-height))
	  (dx (if (= (nth 0 (window-edges)) 0) 1
		-1))
	  (dy (if (= (nth 1 (window-edges)) 0) 1
		-1))
	  c)
      (catch 'end-flag
	(while t
	  (message "size[%dx%d]"
		   (window-width) (window-height))
	  (setq c (read-char))
	  (cond ((= c ?l)
		 (enlarge-window-horizontally dx))
		((= c ?h)
		 (shrink-window-horizontally dx))
		((= c ?j)
		 (enlarge-window dy))
		((= c ?k)
		 (shrink-window dy))
		;; otherwise
		(t
		 (message "Quit")
		 (throw 'end-flag t)))))))
  (global-set-key "\C-c\C-r" 'window-resizer)

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

  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  ;; IME
  (if (eq system-type 'windows-nt)
      (setq default-input-method "W32-IME"))

  (if (eq system-type 'gnu/linux)
      (setq default-input-method "japanese-skk"))

  ;; elscreen
  ;; (load "elscreen")
  ;; (setq elscreen-prefix-key (kbd "C-z"))
  ;; (elscreen-start)

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

  ;; emacs-server
  (server-start)

  ;; 行番号表示.
  (require 'linum)
  (global-linum-mode)

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

  ;; backtab
  (defun backtab()
    "Do reverse indentation"
    (interactive)
    (back-to-indentation)
    (delete-backward-char
     (if (< (current-column) (car tab-stop-list)) 0
       (- (current-column)
	  (car (let ((value (list 0)))
		 (dolist (element tab-stop-list value)
		   (setq value (if (< element (current-column)) (cons element value) value)))))))))

  ;; transparent window
  (if window-system (progn
		      (set-frame-parameter nil 'alpha 90) ;透明度
		      ))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode pug-mode minitest hide-comnt yapfify ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit spacemacs-theme spaceline smeargle slim-mode scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rake rainbow-delimiters racer quelpa pyvenv pytest pyenv-mode py-isort popwin pip-requirements persp-mode paradox orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file omnisharp neotree move-text mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc jade-mode info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio go-eldoc gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md ggtags flycheck-rust flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu emmet-mode elisp-slime-nav dumb-jump disaster define-word cython-mode company-web company-tern company-statistics company-go company-emacs-eclim company-c-headers company-anaconda column-enforce-mode coffee-mode cmake-mode clean-aindent-mode clang-format chruby cargo bundler auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
    (minitest hide-comnt powershell yapfify ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit spacemacs-theme spaceline smeargle slim-mode scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rake rainbow-delimiters racer quelpa pyvenv pytest pyenv-mode py-isort pug-mode popwin pip-requirements persp-mode paradox orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file omnisharp neotree move-text mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio go-eldoc gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md ggtags flycheck-rust flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu emmet-mode elisp-slime-nav dumb-jump disaster define-word cython-mode company-web company-tern company-statistics company-go company-emacs-eclim company-c-headers company-anaconda column-enforce-mode coffee-mode cmake-mode clean-aindent-mode clang-format chruby cargo bundler auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
