;; Add a directory to Emacs PATH
(let ((my/path (expand-file-name "~/.nix-profile/bin")))
  (setenv "PATH" (concat my/path ":" (getenv "PATH")))
  (add-to-list 'exec-path my/path))

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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; general
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

;; font
(set-face-attribute 'default nil
					:font "JetBrainsMono Nerd Font"
                    :height 120
                    :weight 'medium)
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font"))
(setq-default line-spacing 0.12)

;; transparent background
(set-frame-parameter (selected-frame) 'alpha-background 80)
(add-to-list 'default-frame-alist '(alpha-background . 80))

;; nerd font
(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; evil mode
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

;; keybinds
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
										;"q" '(flymake-show-buffer-diagnostics :wk "Flymake buffer diagnostic")
    "g" '(magit-status :wk "Magit status")
	"e" '(dired-jump :wk "Open dired at current buffer")
    "T" '(eat :wk "Eat terminal")
	"c" '(kill-current-buffer :wk "Kill current buffer")
    "Q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
    "R" '((lambda () (interactive)
			(load-file "~/.config/emacs/init.el"))
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

;; eat
(add-hook 'eat-mode-hook
		  (lambda () (setq-local truncate-lines t)
			(visual-line-mode -1)))


;; lang specific modes
(use-package haskell-mode :mode "\\.hs\\'")
(use-package kdl-mode :mode "\\.kdl\\'")
(use-package lua-mode :mode "\\.lua\\'")
(use-package markdown-mode :mode "\\.md\\'")
(use-package nix-mode :mode "\\.nix\\'")
(use-package web-mode :mode ("\\.html?\\'" "\\.css\\'"  "\\.js\\'" "\\.json\\'"))

(use-package org
  :ensure nil
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
  :hook (org-mode . org-superstar-mode))

(use-package apheleia
  :config
  (add-to-list 'apheleia-mode-alist '(nix-mode . alejandra))
  (apheleia-global-mode +1))



(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode)
  :custom
  (sideline-flymake-display-mode 'line) ;; Show errors on the current line
  (sideline-backends-right '(sideline-flymake)))
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum length of prefix for auto completion.
  (corfu-popupinfo-mode t)       ;; Enable popup information
  (corfu-popupinfo-delay 0.5)    ;; Lower popup info delay to 0.5 seconds from 2 seconds
  (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
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

(use-package cape
  :after corfu
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
  (add-hook 'completion-at-point-functions #'cape-dict) ;; Dictionary completion
  (add-hook 'completion-at-point-functions #'cape-file) ;; Path completion
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
  (add-hook 'completion-at-point-functions #'cape-keyword) ;; Keyword completion
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol) ;; Complete Elisp symbol
  (add-hook 'completion-at-point-functions #'cape-tex) ;; Complete Unicode char from TeX command, e.g. \hbar
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

;; latex
(use-package auctex
  :ensure t
  :defer t
  )
(setq TeX-view-program-list
      '(("Zathura" "zathura %o")))
(setq TeX-view-program-selection
      '((output-pdf "Zathura")
        (output-dvi "xdvi")
        (output-html "xdg-open")))
(setq TeX-engine 'luatex)

;; magit
(use-package magit
  :defer
  :custom (magit-diff-refine-hunk (quote all)) ;; Shows inline diff
  :config (define-key transient-map (kbd "<escape>") 'transient-quit-one) ;; Make escape quit magit prompts
  )

(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode))

;; help!
(use-package helpful
  :bind
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  )

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

;; colorful brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; increase the amount of data which emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb
