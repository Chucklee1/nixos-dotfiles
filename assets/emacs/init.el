(use-package emacs
  :custom
  (menu-bar-mode nil)         ;; Disable the menu bar
  (scroll-bar-mode nil)       ;; Disable the scroll bar
  (tool-bar-mode nil)         ;; Disable the tool bar
  (inhibit-startup-screen t)  ;; Disable welcome screen

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
         ("<C-wheel-down>" . text-scale-decrease)
         )
  )

(add-hook 'window-setup-hook (lambda ()
		  (set-frame-parameter (selected-frame) 'alpha-background 80)
		  (add-to-list 'default-frame-alist '(alpha-background . 80))))

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

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
  ;; 'SPC' as vim leader, 'C-SPC' as global leader
  (general-create-definer start/leader-keys
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (start/leader-keys
    "." '(find-file :wk "Find file")
    "TAB" '(comment-line :wk "Comment lines")
    "g" '(magit-status :wk "Magit status")
    "e" '(dired-jump :wk "Open dired at current buffer")
    "T" '(eat :wk "Eat terminal")
	"c" '(kill-current-buffer :wk "Kill current buffer")
    "Q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
    "R" '((lambda () (interactive)
			(load-file CONFIG_PATH))
          :wk "Reload Emacs config"))

  (start/leader-keys
    "b" '(:ignore t :wk "Buffers")
    "b i" '(ibuffer :wk "Ibuffer")
	"b r" '(revert-buffer :wk "Reload buffer"))

  (general-define-key
   :states '(normal visual motion emacs)
   :keymaps 'override
   "L" '(next-buffer :wk "Next buffer")
   "H" '(previous-buffer :wk "Previous buffer")))

(use-package dired
  :straight (:type built-in)
  :ensure nil
  :config
  (general-define-key
   :keymaps 'dired-mode-map
   "<C-right>" 'dired-find-file
   "<C-left>" 'dired-up-directory)

  ;; Remove cursor and use highlight line
  (add-hook 'dired-mode-hook
			(lambda ()
			  (setq cursor-type nil)        ;; Hide cursor
			  (hl-line-mode 1))))           ;; Highlight current line

(add-hook 'eat-mode-hook (lambda ()
						   (setq-local truncate-lines t)
						   (visual-line-mode -1)))

(use-package haskell-mode :mode "\\.hs\\'")
(use-package kdl-mode :mode "\\.kdl\\'")
(use-package lua-mode :mode "\\.lua\\'")
(use-package markdown-mode :mode "\\.md\\'")
(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (nix-mode . (lambda ()
					  (add-hook 'before-save-hook #'nix-mode-format nil t))))
(use-package web-mode :mode ("\\.html?\\'" "\\.css\\'"  "\\.js\\'" "\\.json\\'"))

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

(add-hook 'org-mode-hook
		  (lambda ()
			;; Turn on variable-pitch for the buffer
			(variable-pitch-mode 1)

			;; Set the variable-pitch (body text) font
			(set-face-attribute 'variable-pitch nil :family "Noto Sans CJK TC" :height 120)

			;; Keep fixed-pitch faces for code blocks, tables, etc.
			(dolist (face '(org-block
							org-block-begin-line
							org-block-end-line
							org-code
							org-verbatim
							org-meta-line
							org-special-keyword
							org-table))
			  (set-face-attribute face nil :family "JetBrainsMono Nerd Font" :height 120))))

(require 'ox-latex)

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

(use-package auctex
  :ensure t
  :defer t
  )
(setq TeX-view-program-list
      '(("Zathura" "zathura %")))
(setq TeX-view-program-selection
      '((output-pdf "Zathura")
        (output-dvi "xdvi")
        (output-html "xdg-open")))
(setq TeX-engine 'luatex)

(use-package tree-sitter
  :hook ((prog-mode . turn-on-tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode t))

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
