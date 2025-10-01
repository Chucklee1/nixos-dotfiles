;; functions ;;
(defun my/when-!nixos (cmd)
  (when (not (string-match-p "NixOS"
						   (shell-command-to-string "cat /etc/os-release")))
  cmd))
  ;; variables ;;
(defvar g/fheight (if (eq system-type 'darwin) 150 130))
(defvar g/ffamily "JetBrainsMono Nerd Font Propo")
(defvar g/opacity (if (eq system-type 'darwin) 40 90))

(use-package emacs
  :custom
  ;; ui
  (menu-bar-mode nil)         ;; Disable the menu bar
  (scroll-bar-mode nil)       ;; Disable the scroll bar
  (tool-bar-mode nil)         ;; Disable the tool bar
  (inhibit-startup-screen t)  ;; Disable welcome screen
  (initial-buffer-choice (lambda () (dired "~/")))
  (blink-cursor-mode nil)     ;; Don't blink cursor

  ;; selection
  (use-short-answers t)       ;; Use y/n instead of yes/no
  (delete-selection-mode t)   ;; Select text and delete it by typing.

  ;; behavior
  (electric-indent-mode nil)  ;; Turn off the weird indenting that Emacs does by default.
  (tab-width 4)


  (global-auto-revert-mode t) ;; Automatically reload file and show changes if the file has changed

  (dired-kill-when-opening-new-dired-buffer t) ;; Dired don't create new buffer
  (recentf-mode t) ;; Enable recent file mode

  ;; scrolling
  (mouse-wheel-progressive-speed nil) ;; Disable progressive speed when scrolling
  (scroll-conservatively 10) ;; Smooth scrolling
  (scroll-margin 8)


  (savehist-mode) ;; Enables save history mode
  (make-backup-files nil) ;; Stop creating ~ backup files
  (auto-save-default nil) ;; Stop creating # auto save files

  :hook
  (prog-mode   . (lambda () (display-line-numbers-mode t)))
  (text-mode   . (lambda () (display-line-numbers-mode t)))
  (org-mode    . (lambda () (display-line-numbers-mode nil)))
  (before-save . (lambda () (delete-trailing-whitespace)))
  :config
  ;; Move customization variables to a separate file and load it, avoid filling up init.el with unnecessary variables
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  :bind (
		 ([escape] . keyboard-escape-quit) ;; Makes Escape quit prompts (Minibuffer Escape)
		 ;; Zooming In/Out
		 ("C-+" . text-scale-increase)
		 ("C--" . text-scale-decrease)
		 ("<C-wheel-up>" . text-scale-increase)
		 ("<C-wheel-down>" . text-scale-decrease)))

(use-package evil
  :init
  (evil-mode)
  :custom
  (evil-want-keybinding nil)    ;; Disable evil bindings in other modes (It's not consistent and not good)
  (evil-want-C-u-scroll t)      ;; Set C-u to scroll up
  (evil-want-C-i-jump nil)      ;; Disables C-i jump
  (evil-undo-system 'undo-redo) ;; C-r to redo
  ;; Unmap keys in 'evil-maps. If not done, org-return-follows-link will not work
  :bind (:map evil-motion-state-map
              ("SPC" . nil)
              ("RET" . nil)
			  ("TAB" . nil)))
(use-package evil-collection
  :after evil
  :config
  ;; Setting where to use evil-collection
  (setq evil-collection-mode-list '(dired ibuffer magit corfu vertico consult info))
  (evil-collection-init))

(use-package general
  :config
  (general-evil-setup)

  (general-create-definer emacs/leader
	:states '(normal Special Messages org)
	:keymaps 'override
	:prefix "C-")

  (general-create-definer vim/leader
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (vim/leader
    "."   '(find-file :wk "Find file")
    "TAB" '(comment-line :wk "Comment lines")
	"RET" '(term :wk "terminal")
    "g"   '(magit-status :wk "Magit status")
    "e"   '(dired-jump :wk "Dired at Current Buffer")
    "w"   '(evil-write :wk "Write Current Buffer")
    "Q"   '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
    "R"   '((lambda () (interactive)
			  (load-file CONFIG_PATH))
			:wk "Reload Emacs config"))

  (emacs/leader
	"C-<TAB>" '(company-indent-or-complete-common))

  (vim/leader
    "b"   '(:ignore t :wk "Buffers")
    "b i" '(ibuffer :wk "Ibuffer")
    "b d" '(kill-current-buffer :wk "Buffer Delete")
    "b D" '(kill-buffer (current-buffer) :wk "Buffer Delete Forced")
    "b r" '(revert-buffer :wk "Reload buffer"))

  (vim/leader
	"o"     '(:ignore t :wk "Org")
	"o a"   '(org-agenda-list :wk "Agenda")
	"o t"   '(org-todo :wk "Mark as TODO/DONE/nothing")
	"o l" '(org-latex-preview :wk "Preview LaTeX stuff"))

  (vim/leader
    "t" '(:ignore t :wk "Toggle")
    "t i" '(org-toggle-inline-images :wk "Org Inline Images")
    "t n" '(display-line-numbers-mode 'toggle :wk "Buffer Numberline")
    "t N" '(global-display-line-numbers-mode 'toggle :wk "Global Numberline")
    "t b" '(global-tab-line-mode 'toggle :wk "Global Tabline")))

(general-define-key
 :states '(normal motion)
 :keymaps 'dired-mode-map
 "h" 'dired-up-directory
 "<left>" 'dired-up-directory
 "l" 'dired-find-file
 "<right>" 'dired-find-file
 "TAB" 'dirvish-subtree-toggle)

(general-define-key
 :states '(normal Special Messages)
 :keymaps 'override
 "H" '(previous-buffer :wk "Previous buffer")
 "<S-left>" '(previous-buffer :wk "Previous buffer")
 "L" '(next-buffer :wk "Next buffer")
 "<S-right>" '(next-buffer :wk "Next buffer"))

(use-package dirvish
  :config
  (dirvish-override-dired-mode))

(use-package doom-themes
:custom
;; Global settings (defaults)
(doom-themes-enable-bold t)   ; if nil, bold is universally disabled
(doom-themes-enable-italic t) ; if nil, italics is universally disabled
:config
(load-theme 'doom-nord t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config))

(add-hook 'window-setup-hook (lambda ()
		  (set-frame-parameter (selected-frame) 'alpha-background g/opacity)
		  (add-to-list 'default-frame-alist '(alpha-background . g/opacity))))

(set-face-attribute 'default nil
                    :font   g/ffamily
                    :height g/fheight
                    :weight 'medium)
(setq-default line-spacing 0.12)

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package doom-modeline
  :custom
  (display-time-24hr-format t)
  (display-time-mode t)
  (doom-modeline-spc-face-overrides nil)
  (doom-modeline-buffer-encoding nil)
  :hook (after-init . doom-modeline-mode))

(use-package projectile
  :config
  (projectile-mode)
  :custom
  (projectile-run-use-comint-mode t)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-project-search-path '("~/Documents/" "~/Repos/")))

(use-package eglot
  :ensure nil ;; Don't install eglot because it's now built-in
  :hook ((c-mode
		  c++-mode
		  haskell-mode
		  kdl-mode
		  lua-mode
		  markdown-mode
		  nix-mode
		  qml-mode
		  web-mode)
         . eglot-ensure)
  :custom
  ;; Good default
  (eglot-events-buffer-size 0) ;; No event buffers (LSP server logs)
  (eglot-autoshutdown t);; Shutdown unused servers.
  (eglot-report-progress nil) ;; Disable LSP server logs (Don't show lsp messages at the bottom, java)
  )

(use-package tree-sitter
  :hook ((prog-mode . turn-on-tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package haskell-mode :mode "\\.hs\\'")
(use-package kdl-mode :mode "\\.kdl\\'")
(use-package lua-mode :mode "\\.lua\\'")
(use-package markdown-mode :mode "\\.md\\'")
(use-package nix-mode :mode "\\.nix\\'")
(use-package qml-mode :mode ("\\.qml\\'" "\\.qss\\'"))
(use-package web-mode :mode ("\\.html?\\'" "\\.css\\'"  "\\.js\\'" "\\.json\\'"))

(use-package org
  :custom
  (org-return-follows-link t)   ;; Sets RETURN key in org-mode to follow links
  :hook
  (org-mode . org-indent-mode)
  (org-mode . (lambda () (setq
						  ;; Edit settings
						  org-auto-align-tags nil
						  org-tags-column 0
						  org-catch-invisible-edits 'show-and-error
						  org-special-ctrl-a/e t
						  org-insert-heading-respect-content t

						  ;; Org styling, hide markup etc.
						  org-hide-emphasis-markers t
						  org-pretty-entities t
						  org-agenda-tags-column 0
						  org-ellipsis "…")
				)))

(defun config/sync-with-org ()
  (when (string-equal (file-truename buffer-file-name)
					  (file-truename ORGCFG_PATH))
	(org-babel-tangle)))

(add-hook 'org-mode-hook
		  (lambda ()
			(add-hook 'after-save-hook
					  (lambda ()
						(config/sync-with-org))
					  nil t)))

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

;; Minimal UI
(use-package org-modern)


(global-org-modern-mode)
(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package auctex
  :ensure t
  :defer t)

(setq TeX-view-program-selection
      '((output-pdf "Zathura")
        (output-dvi "xdvi")
        (output-html "xdg-open")))
(setq TeX-engine 'luatex)

(defun my/org-to-pdf-view ()
  (interactive)
  (let ((pdf-file (org-latex-export-to-pdf)))
	(when pdf-file
	  (setq TeX-master pdf-file)
	  (TeX-view))))

(use-package company
  :custom
  (company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.5)))
  :hook
  (after-init . global-company-mode))

(use-package cape
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.

  ;; The functions that are added later will be the first in the list
  (add-hook 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
  (add-hook 'completion-at-point-functions #'cape-dict) ;; Dictionary completion
  (add-hook 'completion-at-point-functions #'cape-file) ;; Path completion
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
  (add-hook 'completion-at-point-functions #'cape-keyword) ;; Keyword completion

  ;;(add-hook 'completion-at-point-functions #'cape-abbrev) ;; Complete abbreviation
  ;;(add-hook 'completion-at-point-functions #'cape-history) ;; Complete from Eshell, Comint or minibuffer history
  ;;(add-hook 'completion-at-point-functions #'cape-line) ;; Complete entire line from current buffer
  ;;(add-hook 'completion-at-point-functions #'cape-elisp-symbol) ;; Complete Elisp symbol
  ;;(add-hook 'completion-at-point-functions #'cape-tex) ;; Complete Unicode char from TeX command, e.g. \hbar
  ;;(add-hook 'completion-at-point-functions #'cape-sgml) ;; Complete Unicode char from SGML entity, e.g., &alpha
  ;;(add-hook 'completion-at-point-functions #'cape-rfc1345) ;; Complete Unicode char using RFC 1345 mnemonics
  )

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  ('marginalia-mode-hook . 'nerd-icons-completion-marginalia-setup))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package magit
  :defer
  :custom (magit-diff-refine-hunk (quote all)) ;; Shows inline diff
  :config (define-key transient-map (kbd "<escape>") 'transient-quit-one)) ;; Make escape quit magit prompts

(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode))

(use-package helpful
  :bind
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command))

(use-package which-key
  :ensure nil ;; Don't install which-key because it's now built-in
  :init
  (which-key-mode 1)
  :diminish
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha) ;; Same as default, except single characters are sorted alphabetically
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1) ;; Number of spaces to add to the left of each column
  (which-key-min-display-lines 6)  ;; Increase the minimum lines to display because the default is only 1
  (which-key-idle-delay 0.8)       ;; Set the time delay (in seconds) for the which-key popup to appear
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit nil)) ;; Fixes which-key window slipping out in Emacs Daemon

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode)
(setq rainbow-x-colors nil)

(when (eq system-type 'darwin)
  (use-package ultra-scroll
	:init
	(setq scroll-margin 0) ; important: scroll-margin greater than 0 not yet supported
	:config
	(ultra-scroll-mode 1)))
