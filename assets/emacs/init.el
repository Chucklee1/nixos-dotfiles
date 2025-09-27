(use-package emacs
  :custom
  (menu-bar-mode nil)         ;; Disable the menu bar
  (scroll-bar-mode nil)       ;; Disable the scroll bar
  (tool-bar-mode nil)         ;; Disable the tool bar
  (inhibit-startup-screen t)  ;; Disable welcome screen
  (initial-buffer-choice (lambda () (dired "~/")))
  (use-short-answers t)       ;; Use y/n instead of yes/no

  (delete-selection-mode t)   ;; Select text and delete it by typing.
  (electric-indent-mode nil)  ;; Turn off the weird indenting that Emacs does by default.
  (savehist-mode) ;; Enables save history mode

  (blink-cursor-mode nil)     ;; Don't blink cursor
  (global-auto-revert-mode t) ;; Automatically reload file and show changes if the file has changed

  (dired-kill-when-opening-new-dired-buffer t) ;; Dired don't create new buffer
  (recentf-mode t) ;; Enable recent file mode

  (mouse-wheel-progressive-speed nil) ;; Disable progressive speed when scrolling
  (scroll-conservatively 10) ;; Smooth scrolling
  (scroll-margin 8)

  (tab-width 4)

  (make-backup-files nil) ;; Stop creating ~ backup files
  (auto-save-default nil) ;; Stop creating # auto save files
  :hook
  (prog-mode . (lambda () (display-line-numbers-mode t)))
  (text-mode . (lambda () (display-line-numbers-mode t)))
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
  :config
  (evil-set-initial-state 'eat-mode 'insert) ;; Set initial state in eat terminal to insert mode
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

  (general-create-definer noleader
   :states '(normal Special Messages)
   :keymaps 'override)

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

  (vim/leader
    "t" '(:ignore t :wk "Toggle")
    "t i" '(org-toggle-inline-images :wk "Org Inline Images")
    "t n" '(display-line-numbers-mode 'toggle :wk "Buffer Numberline")
    "t N" '(global-display-line-numbers-mode 'toggle :wk "Global Numberline")
    "t b" '(global-tab-line-mode 'toggle :wk "Global Tabline"))

(use-package dirvish
  :config
  (dirvish-override-dired-mode))

(straight-use-package
 '(eat :type git
	  :host codeberg
	  :repo "akib/emacs-eat"
	  :files ("*.el" ("term" "term/*.el") "*.texi"
			  "*.ti" ("terminfo/e" "terminfo/e/*")
			  ("terminfo/65" "terminfo/65/*")
			  ("integration" "integration/*")
			  (:exclude ".dir-locals.el" "*-tests.el"))))

(add-hook 'eat-mode-hook (lambda ()
						   (setq-local truncate-lines t)
						   (visual-line-mode -1)))

(when (not (string-match-p "NixOS"
                         (shell-command-to-string "cat /etc/os-release")))
  (use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)))

(add-hook 'window-setup-hook (lambda ()
		  (set-frame-parameter (selected-frame) 'alpha-background 80)
		  (add-to-list 'default-frame-alist '(alpha-background . 80))))

(defun set-default-font (face height)
  "Set's default font attributes"
  (set-face-attribute face nil
					  :family "JetBrainsMono Nerd Font Propo"
					  :height height))



(set-default-font 'default 130)

;; MacOS - bigger font
(when (eq system-type 'darwin)
  (set-default-font 'default 150))

(add-hook 'org-mode-hook
		  (lambda ()
			(variable-pitch-mode 1)
			;; body font
			(set-face-attribute 'variable-pitch nil
								:family "Noto Sans Mono CJK TC"
								:height 140
								:weight 'normal)
			;; fixed-pitch for blocks
			(dolist (face
					 '(org-block org-block-begin-line org-block-end-line
								 org-code org-verbatim org-meta-line
								 org-special-keyword org-table))
			  (set-default-font face 130))

			;; MacOS Overrides
			(when (eq system-type 'darwin)
			  (set-default-font 'variable-pitch 150)
			(dolist (face
					 '(org-block org-block-begin-line org-block-end-line
								 org-code org-verbatim org-meta-line
								 org-special-keyword org-table))
			  (set-default-font face 150)))))

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(setq doom-modeline-buffer-encoding nil)

(use-package tree-sitter
  :hook ((prog-mode . turn-on-tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package haskell-mode :mode "\\.hs\\'")
(use-package kdl-mode :mode "\\.kdl\\'")
(use-package lua-mode :mode "\\.lua\\'")
(use-package markdown-mode :mode "\\.md\\'")
(use-package qml-mode :mode ("\\.qml\\'" "\\.qss\\'"))
(use-package web-mode :mode ("\\.html?\\'" "\\.css\\'"  "\\.js\\'" "\\.json\\'"))

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :hook (nix-mode . (lambda ()
					  (add-hook 'before-save-hook #'nix-mode-format nil t))))

(use-package org
  :ensure nil ;; provided by nixpkgs
  :custom
  (org-edit-src-content-indentation 4) ;; Set src block automatic indent to 4 instead of 2.
  (org-return-follows-link t)   ;; Sets RETURN key in org-mode to follow links
  :hook
  (org-mode . org-indent-mode) ;; Indent text
  )
(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))
(use-package org-superstar
  :after org
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "⚬" "◈" "◇"))
  :hook (org-mode . org-superstar-mode))

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

(use-package org-super-agenda
			  :after org-agenda
			  :init
			  (org-super-agenda-mode)
			  :config
			  (setq org-super-agenda-header-map (make-sparse-keymap)))

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

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode t))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package corfu
  :init
  (global-corfu-mode))

(setq read-extended-command-predicate #'command-completion-default-include-p)

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum length of prefix for auto completion.
  (corfu-popupinfo-mode t)       ;; Enable popup information
  (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (completion-ignore-case t)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  (corfu-preview-current nil) ;; Don't insert completion without confirmation
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

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
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(use-package rainbow-mode)
(setq rainbow-x-colors nil)

(when (eq system-type 'darwin)
  (use-package ultra-scroll
	:init
	(setq scroll-margin 0) ; important: scroll-margin greater than 0 not yet supported
	:config
	(ultra-scroll-mode 1)))
