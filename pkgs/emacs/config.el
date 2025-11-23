(setq use-package-always-ensure t)
(defconst CONFIG_PATH (expand-file-name "~/.emacs.d/init.el"))
(defconst ORGCFG_PATH (expand-file-name "~/.emacs.d/init.org"))

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

  (blink-cursor-mode nil)     ;; Don't blink cursor

  ;; selection
  (use-short-answers t)       ;; Use y/n instead of yes/no
  (delete-selection-mode t)   ;; Select text and delete it by typing.

  ;; behavior
  (electric-indent-mode nil)  ;; Turn off the weird indenting that Emacs does by default.
  (tab-width 4)

  (global-auto-revert-mode t) ;; Automatically reload file and show changes if the file has changed

  (recentf-mode t) ;; Enable recent file mode

  ;; scrolling
  (mouse-wheel-progressive-speed nil) ;; Disable progressive speed when scrolling
  (scroll-conservatively 10) ;; Smooth scrolling
  (scroll-margin 8)


  (savehist-mode) ;; Enables save history mode
  (make-backup-files nil) ;; Stop creating ~ backup files
  (auto-save-default nil) ;; Stop creating # auto save files

  :hook
  (before-save   . (lambda () (delete-trailing-whitespace)))
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

(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'org-mode-hook #'outline-minor-mode) ;; Org already uses outline

;; Optional: remap folding keys to Evil style in Org
(with-eval-after-load 'evil
  (evil-set-initial-state 'outline-mode 'normal))

(defvar lmap-globl (make-sparse-keymap))
(evil-define-key '(normal motion) 'evil-normal-state-map
  (kbd "SPC") lmap-globl)

(evil-define-key '(normal visual) 'evil-normal-state-map
(kbd "H")         'previous-buffer
(kbd "<S-left>")  'previous-buffer
(kbd "L")         'next-buffer
(kbd "<S-right>") 'next-buffer)

;; globals
(define-key lmap-globl (kbd "TAB") 'comment-line)
(define-key lmap-globl (kbd "w")   'save-buffer)
(define-key lmap-globl (kbd "R")
            (lambda () (interactive)
              (load-file CONFIG_PATH)))

;; programs
(define-key lmap-globl (kbd "e") 'dired-jump)
(define-key lmap-globl (kbd "G") 'magit-status)

(defvar lmap-globl/buffer (make-sparse-keymap))
(define-key lmap-globl (kbd "b") lmap-globl/buffer)
;; actual key defs
(define-key lmap-globl/buffer (kbd "i") 'ibuffer)
(define-key lmap-globl/buffer (kbd "d") 'kill-current-buffer)
(define-key lmap-globl/buffer (kbd "D")
       		(lambda () (interactive)
       		  (kill-buffer (current-buffer))))
(define-key lmap-globl/buffer (kbd "r") 'revert-buffer)

(defvar lmap-globl/org (make-sparse-keymap))
(define-key lmap-globl (kbd "o") lmap-globl/org)
;; general
(define-key lmap-globl/org (kbd "i") 'org-toggle-inline-images)
(define-key lmap-globl/org (kbd "t") 'org-todo)
(define-key lmap-globl/org (kbd "s") 'org-schedule)
(define-key lmap-globl/org (kbd "d") 'org-deadline)
;; agenda
(defvar lmap-globl/org/agenda (make-sparse-keymap))
(define-key lmap-globl/org (kbd "a") lmap-globl/org/agenda)
(define-key lmap-globl/org/agenda (kbd "a") 'org-agenda-list)
;; babel
(defvar lmap-globl/org/babel (make-sparse-keymap))
(define-key lmap-globl/org (kbd "b") lmap-globl/org/babel)
(define-key lmap-globl/org/babel (kbd "t") 'org-babel-tangle)
;; LaTeX
(defvar lmap-globl/org/latex (make-sparse-keymap))
(define-key lmap-globl/org (kbd "l") lmap-globl/org/latex)
(define-key lmap-globl/org (kbd "p") 'org-latex-preview)

(defvar lmap-globl/toggle (make-sparse-keymap))
(define-key lmap-globl (kbd "t") lmap-globl/toggle)
;; actual key defs
(define-key lmap-globl/toggle (kbd "n")
  			(lambda () (interactive)
  			  (display-line-numbers-mode 'toggle)))
(define-key lmap-globl/toggle (kbd "N")
  		    (lambda () (interactive)
  			  (global-display-line-numbers-mode 'toggle)))
(define-key lmap-globl/toggle (kbd "b")
  			(lambda () (interactive)
  			  (global-tab-line-mode 'toggle)))

(use-package dired
  :ensure nil ;; builtin
  :custom
  (dired-omit-files "^\\..*$") ;; hide . & .. folders
  :config
  (setq
   dired-listing-switches "-lAh --group-directories-first"
   delete-by-moving-to-trash t
   ;; Dired don't create new buffer
   dired-kill-when-opening-new-dired-buffer t))

(use-package dired-subtree
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil)
  :bind
  ( :map dired-mode-map
    ("TAB" . dired-subtree-toggle)
    ("<tab>" . dired-subtree-toggle))
  :config
  ;; Fix "no icons in subtree" issue.
  (defadvice dired-subtree-toggle
      (after add-icons activate) (revert-buffer)))

(use-package dired-collapse
  :hook (dired-mode . global-dired-collapse-mode))

(use-package diredfl :hook ((dired-mode . diredfl-mode)))

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

(use-package haskell-mode :mode "\\.hs\\'")
(use-package kdl-mode :mode "\\.kdl\\'")
(use-package kotlin-mode :mode "\\.kt\\'")
(use-package lua-mode :mode "\\.lua\\'")
(use-package markdown-mode :mode "\\.md\\'")
(use-package nix-mode :mode "\\.nix\\'")
(use-package qml-mode :mode ("\\.qml\\'" "\\.qss\\'"))
(use-package web-mode :mode ("\\.html?\\'" "\\.css\\'"  "\\.js\\'" "\\.json\\'"))

(use-package auctex)

(setq TeX-view-program-selection
      '((output-pdf "Zathura")
        (output-dvi "xdvi")
        (output-html "xdg-open")))

(use-package org
  :custom
  (org-return-follows-link t)   ;; Sets RETURN key in org-mode to follow links
  :hook
  (org-mode . (lambda ()
				(setq
				 ;; Edit settings
  				 org-catch-invisible-edits 'show-and-error
  				 org-special-ctrl-a/e t ;; smart jump keys
  				 org-insert-heading-respect-content t
  				 ;; Org styling, hide markup etc.
  				 org-hide-emphasis-markers t
  				 org-pretty-entities t
  				 org-ellipsis "…" ;; use ... for folded text
  				 ))))

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

(use-package org-modern)
(setq org-modern-star ["●" "○" "◆" "◇" "▶" "▷"])
(global-org-modern-mode)

(setq org-latex-create-formula-image-program 'dvisvgm)

(setq org-preview-latex-default-process 'dvisvgm)

(setq org-preview-latex-process-alist
      '((dvisvgm
         :programs ("latex" "dvisvgm")
         :description "Convert LaTeX fragments to SVG using dvisvgm"
         :message "Creating SVG image from LaTeX fragment..."
         :image-input-type "dvi"
         :image-output-type "svg"
         :image-size-adjust (2.5 . 2.5)
         :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
         :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))))

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

(use-package company
  :custom
  (company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.5)))
  :hook
  (after-init . global-company-mode))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
  (add-hook 'completion-at-point-functions #'cape-dict) ;; Dictionary completion
  (add-hook 'completion-at-point-functions #'cape-file) ;; Path completion
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
  (add-hook 'completion-at-point-functions #'cape-keyword) ;; Keyword completion
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
  :custom (magit-diff-refine-hunk (quote all)) ;; Shows inline diff
  :config (define-key transient-map (kbd "<escape>") 'transient-quit-one)) ;; Make escape quit magit prompts

(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode))

(use-package helpful
  :bind
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

(use-package rainbow-mode
  :config (setq rainbow-x-colors nil)
  :hook (prog-mode . rainbow-mode))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(when (eq system-type 'darwin)
  (use-package ultra-scroll
	:init
	(setq scroll-margin 0) ; important: scroll-margin greater than 0 not yet supported
	:config
	(ultra-scroll-mode 1)))
