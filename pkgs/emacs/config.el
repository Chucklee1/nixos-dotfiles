;; -*- lexical-binding: t; -*-

(defvar g/path/elispcfg (expand-file-name "~/.emacs.d/init.el"))
(defvar g/path/orgcfg   (expand-file-name "~/.emacs.d/init.org"))
(defvar g/fheight       (if (eq system-type 'darwin) 150 130))
(defvar g/ffamily       "JetBrainsMono Nerd Font Propo")
(defvar g/opacity       (if (eq system-type 'darwin) 40 80))

;; keybinds ;;
(defun mkkeygroup (leader group-name group-key keypairs)
  "Defines a set of keybinds with a root leader and a sub-leader, note the group-name requires a defined sparemap beforehand"
  (interactive)
  (define-key leader (kbd group-key) group-name)
  (dolist (pair keypairs)
    ;; pair format => ("key" . cmd)
    (define-key group-name (kbd (car pair)) (cdr pair))))

(defun helper/buffer/toggle (cmd)
  (interactive)
  (let* ((buf-name (concat "*" cmd "*"))
         (buf      (get-buffer buf-name)))
    (if buf
        (progn
          (when-let ((win (get-buffer-window buf)))
            (delete-window win))
          (kill-buffer buf))
      (call-interactively (intern cmd)))))

(defun helper/open-split-term ()
  (interactive)
  (let* ((height (floor (* 0.25 (window-total-height))))
         (new-win (split-window-below (- height))))
    (select-window new-win)
    (vterm (getenv "SHELL"))))

;; theme ;;
(defun doom/colors (color)
  (cadr (assoc color doom-themes--colors)))

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
  (indent-tabs-mode nil) ;; use spaces instead of tabs
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
  (before-save   . delete-trailing-whitespace)
  (prog-mode     . (lambda () (display-line-numbers-mode 1)))
  ;; (window-setup  . toggle-frame-maximized)
  :config
  ;; Move customization variables to a separate file and load it, avoid filling up init.el with unnecessary variables
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  :bind (
         ([escape] . keyboard-escape-quit) ;; Makes Escape quit prompts (Minibuffer Escape)
         ;; Zooming In/Out
         ("C-=" . text-scale-increase)
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

(dolist (pair
         '(("RET" . helper/open-split-term)
           ("R"   . (lambda () (interactive) (load-file g/path/elispcfg)))
           ("i"   . imenu)))
         (define-key lmap-globl (kbd (car pair)) (cdr pair)))

(defvar lmap-globl/buffer (make-sparse-keymap))
(mkkeygroup lmap-globl lmap-globl/buffer "b"
            '(("i" . ibuffer)
              ("r" . revert-buffer)))

(defvar lmap-globl/lsp (make-sparse-keymap))
(mkkeygroup lmap-globl lmap-globl/lsp "l"
            '(("f" . lsp-format-buffer)
              ("r" . lsp-rename)
              ("i" . (lambda () (interactive) (helper/buffer/toggle "lsp-ui-imenu")))))

(defvar lmap-globl/org (make-sparse-keymap))
(mkkeygroup lmap-globl lmap-globl/org "o"
            '(("a" . org-agenda-list)
              ("t" . org-babel-tangle)
              ("l" . org-latex-preview)
              ("i" . org-toggle-inline-images))

(defvar lmap-globl/toggle (make-sparse-keymap))
(mkkeygroup lmap-globl lmap-globl/toggle "t"
            '(("b" . global-tab-line-mode)
              ("n" . display-line-numbers-mode)
              ("N" . global-display-line-numbers-mode)
              ("w" . visual-wrap-prefix-mode)
              ("W" . global-visual-wrap-prefix-mode)))

(use-package dired
  :ensure nil ;; builtin
  :custom
  (dired-omit-files "^\\..*$") ;; hide . & .. folders
  :config
  (setq
   dired-listing-switches "-Ahl --group-directories-first"
   delete-by-moving-to-trash t
   ;; Dired don't create new buffer
   dired-kill-when-opening-new-dired-buffer t)
  (when (string= system-type "darwin") ;; macos ls is poop, we need better ls
    (let ((gls (executable-find "gls")))
      (when gls
        (setq dired-use-ls-dired t
              insert-directory-program gls)))))

(use-package dired-collapse
  :hook (dired-mode . global-dired-collapse-mode))

(use-package diredfl :hook ((dired-mode . diredfl-mode)))

(use-package treemacs
  :commands (treemacs)
  :config
  (treemacs-indent-guide-mode t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(when (featurep 'evil)
  (use-package treemacs-evil
    :after (treemacs evil)))

(when (featurep 'magit)
(use-package treemacs-magit
  :after (treemacs magit)))

(when (featurep 'lsp-mode)
(use-package lsp-treemacs
  :after (treemacs)))

(use-package treemacs-nerd-icons
  :after (treemacs lsp-treemacs)
  :hook
  (treemacs-mode . (lambda () (treemacs-load-theme "nerd-icons"))))

(use-package vterm)

(use-package doom-themes
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ;; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ;; if nil, italics is universally disabled
  (doom-themes-treemacs-enable-variable-pitch nil)
  :config
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; treemacs integration
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(defun transparent-window-setup ()

  (set-frame-parameter (selected-frame) 'alpha-background g/opacity)
  (add-to-list 'default-frame-alist `(alpha-background . ,g/opacity))

  ;; must manually set corfu frame-opacity
  (when (featurep 'corfu)
    (with-eval-after-load 'corfu
      (setq corfu-prefer-childframe t)
      (setq corfu-childframe-frame-parameters
            '((alpha-background . ,(+ g/opacity 10))
              (internal-border-width . 1)
              (left-fringe . 5)
              (right-fringe . 5))))))

(add-hook 'window-setup-hook #'transparent-window-setup)


;; unset bg when in terminal/tty
(when (not (display-graphic-p))
  (set-face-background 'default "unspecified-bg"))

(set-face-attribute 'default nil
                    :font   g/ffamily
                    :height g/fheight
                    :weight 'medium)
(setq-default line-spacing 0.12)

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package doom-modeline
  :custom
  (display-time-24hr-format t)
  (display-time-mode t)
  (doom-modeline-spc-face-overrides nil)
  (doom-modeline-buffer-encoding nil)
  :hook (after-init . doom-modeline-mode))

(use-package highlight-indent-guides
  :hook ((prog-mode . highlight-indent-guides-mode)) ; enable in programming modes
  :custom
  (highlight-indent-guides-method 'character) ; or 'column
  (highlight-indent-guides-auto-enabled t))

(use-package evil-terminal-cursor-changer)
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate))

(use-package projectile
  :config
  (projectile-mode)
  :custom
  (projectile-run-use-comint-mode t)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-project-search-path '("~/Documents/" "~/Repos/")))

(use-package lsp-mode
  :commands lsp lsp-deferred
  :hook ((c-ts-mode
          c++-ts-mode
          haskell-mode
          kdl-mode
          lua-mode
          markdown-mode
          nix-ts-mode)
         . lsp-deferred)

  :custom
  ;; performance tweaks
  (read-process-output-max (* 1024 1024)) ; let LSP read more data
  (gc-cons-threshold 100000000)

  ;; Enable semantic tokens support
  (lsp-semantic-tokens-enable t)
  (lsp-semantic-tokens-allow-ranged-requests t)
  (lsp-semantic-tokens-allow-delta-requests t)

  ;; no breadcrumbs
  (lsp-headerline-breadcrumb-enable nil)

  ;; Integrations
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil))

(use-package tree-sitter)
(setq treesit-font-lock-level 4)

(use-package treesit-auto
  :after (tree-sitter)
  :custom
  (treesit-auto-install 't)
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

;; config languages
(use-package kdl-mode :mode "\\.kdl\\'")
(use-package markdown-mode :mode "\\.md\\'")
(use-package ron-mode :mode "\\.ron\\'")
;; there has to be a better way
(if (executable-find "ghc") (use-package haskell-mode))
(if (executable-find "java") (use-package lsp-java))
(if (executable-find "lua") (use-package lua-mode))
(if (executable-find "nu") (use-package nushell-mode))
(if (executable-find "nu") (use-package nushell-mode))
(if (executable-find "fish") (use-package fish-mode))
(if (executable-find "kotlin") (use-package kotlin-mode))
(if (executable-find "gnuplot") (use-package gnuplot-mode))
(if (executable-find "just") (use-package just-mode))

(defun set/clang/version ()
  (let ((raw (shell-command-to-string "clangd --version")))
    (when (string-match "clangd version \\([0-9.]+\\)" raw)
      (setq lsp-clangd-version (match-string 1 raw)))))

(defun set/clang/bin ()
  (let ((clangd-bin (executable-find  "clangd")))
    (setq lsp-clangd-binary-path clangd-bin)
    (setq lsp-clients-clangd-executable clangd-bin)
    (setq lsp-clients--clangd-default-executable clangd-bin)))

(if (executable-find "clangd")
    (add-hook 'c++-ts-mode-hook #'set/clang/version)
  (add-hook 'c++-ts-mode-hook #'set/clang/bin))

(if (executable-find "nix")
    (progn

(use-package nix-mode :mode "\\.nix\\'")
(use-package nix-ts-mode :mode "\\.nix\\'")
(setq lsp-nix-nixd-formatting-command ["alejandra"])

;; provided by nix
(require 'qml-ts-mode)
  ;; taken from quickshell docs
  (add-to-list 'lsp-language-id-configuration '(qml-ts-mode . "qml-ts"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("qmlls"))
                    :activation-fn (lsp-activate-on "qml-ts")
                    :server-id 'qmlls))

))

(if (executable-find "cargo") (use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :after (rust-mode)
  :config
  (setq rustic-format-on-save nil)
  (setq rustic-lsp-client 'lsp-mode)
  :custom
  (rustic-cargo-use-last-stored-arguments t)))

(use-package auctex)

(setq TeX-view-program-selection
      '((output-pdf "Zathura")
        (output-dvi "xdvi")
        (output-html "xdg-open")))

(use-package org
  :custom
  (org-return-follows-link t)   ;; Sets RETURN key in org-mode to follow links
  :config
  (setq org-catch-invisible-edits 'show-and-error
        org-insert-heading-respect-content t
        ;; Org styling, hide markup etc.
        org-hide-emphasis-markers t
        org-pretty-entities t
        ;; use ... for folded text
        org-ellipsis "…"))

(set-face-attribute 'org-document-title nil :height 1.5 :weight 'bold)
(set-face-attribute 'org-document-info nil :height 1.4 :weight 'bold)
(set-face-attribute 'org-level-1 nil :height 1.3 :weight 'bold)
(set-face-attribute 'org-level-2 nil :height 1.2 :weight 'bold)
(set-face-attribute 'org-level-3 nil :height 1.1 :weight 'bold)

(use-package org-modern
  :init (global-org-modern-mode)
  :config
  (setq org-modern-star ["●" "○" "◆" "◇" "▶" "▷"]))

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

(defun helper/org/set-margins ()
  (let ((margin 2))
    (setq-local left-margin-width margin
                right-margin-width margin)
    (set-window-buffer nil (current-buffer))))
(add-hook 'org-mode-hook #'helper/org/set-margins)

;; have horizontal rule ignore magrins
(with-eval-after-load 'org-modern
  (set-face-attribute 'org-modern-horizontal-rule nil :extend t))

(defun config/sync-with-org ()
  (when (string-equal (file-truename buffer-file-name)
                      (file-truename g/path/orgcfg))
    (org-babel-tangle)))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook
                      (lambda ()
                        (config/sync-with-org))
                      nil t)))

(if (executable-find "direnv") (use-package direnv))

(use-package corfu
  :custom
  (corfu-auto nil) ;; no auto-popups
  (corfu-cycle t)  ;; Enable cycling for `corfu-next/previous'
  (text-mode-ispell-word-completion nil)
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("<tab>" . corfu-next)
        ("tab" . corfu-next)
        ("<backtab>" . corfu-previous)
        ("RET" . corfu-insert)
        ("<escape>" . corfu-quit)))

;; setup for rebinding completion to control+tab instead of tab
(defun force/corfu-complete ()
  "Force manual trigger for corfu completion at point"
  (interactive)
  (let ((completion-at-point-functions completion-at-point-functions))
    (completion-at-point)))
(global-set-key (kbd "C-<tab>") #'force/corfu-complete)

;; nerd icon support
(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :after corfu
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
  (add-hook 'completion-at-point-functions #'cape-file) ;; Path completion
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
  (add-hook 'completion-at-point-functions #'cape-keyword) ;; Keyword completion
  (add-hook 'completion-at-point-functions #'cape-tex) ;; Complete Unicode char from TeX command, e.g. \hbar
  )

(use-package flycheck
  :hook after-init global-flycheck-mode
  :config
  (setq flycheck-indication-mode nil))

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; more nerd icon support
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  ('marginalia-mode-hook . 'nerd-icons-completion-marginalia-setup))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

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

;; make flycheck wavy lines not-wavy
(with-eval-after-load 'flycheck
  (set-face-attribute 'flycheck-error nil
                      :inherit 'error
                      :underline t)
  (set-face-attribute 'flycheck-warning nil
                      :inherit 'warning
                      :underline t)
  (set-face-attribute 'flycheck-info nil
                      :inherit 'success
                      :underline t))

(with-eval-after-load 'lsp-modeline
  ;; icon overrides
  (setq lsp-modeline-code-action-fallback-icon "")
  (setq lsp-progress-prefix "")

  ;; color overrides
  (set-face-attribute 'lsp-modeline-code-actions-preferred-face nil
                      :foreground (doom/colors 'yellow))
  (set-face-attribute 'lsp-modeline-code-actions-face nil
                      :foreground (doom/colors 'blue)))

(when (eq system-type 'darwin)
  (progn

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package ultra-scroll
  :init
  (setq scroll-margin 0) ; important: scroll-margin greater than 0 not yet supported
  :config
  (ultra-scroll-mode 1))

))
