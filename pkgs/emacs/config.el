;;; -*- no-byte-compile: t; lexical-binding: t; -*

(setq use-package-always-ensure t)

(setq gc-cons-threshold 100000000) ;; 100 MB

;; Improve performance with language servers.
(setq read-process-output-max (* 1024 1024)) ;; 1 MB

(defvar g/path/elispcfg (expand-file-name "~/.emacs.d/init.el"))
(defvar g/path/orgcfg   (expand-file-name "~/.emacs.d/init.org"))
(defvar g/fheight       (if (eq system-type 'darwin) 150 130))
(defvar g/ffamily       "JetBrainsMono Nerd Font Propo")
(defvar g/opacity/alpha (if (eq system-type 'darwin) 40 80))
(defvar g/opacity/solid 100)
;; default to no transparency
(defvar g/opacity/current  g/opacity/solid)

;; keybinds ;;
(defun helper/mapkeys (leader-map keypairs)
  "map keybinds to a prefixed leader"
  (interactive)
  (dolist (pair keypairs)
    ;; pair format = ("key" . cmd)
    (define-key leader-map (kbd (car pair)) (cdr pair))))

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

;; program(s)
(defun helper/open-split-term ()
  (interactive)
  (let* ((height (floor (* 0.25 (window-total-height))))
         (new-win (split-window-below (- height))))
    (select-window new-win)
    (vterm (getenv "SHELL"))))

;; opacity
(defun helper/opacity/set (opacity)
  (set-frame-parameter (selected-frame) 'alpha-background opacity)
  (add-to-list 'default-frame-alist `(alpha-background . ,opacity))

  ;; must manually set corfu frame-opacity
  (with-eval-after-load 'corfu
    (setq corfu-prefer-childframe t)
    (setq corfu-childframe-frame-parameters
          '((alpha-background . ,(+ opacity 10))
            (internal-border-width . 1)
            (left-fringe . 5)
            (right-fringe . 5)))))

(defun helper/opacity/toggle ()
  (interactive)
  (setq g/opacity/current
        (if (= g/opacity/current g/opacity/solid)
            g/opacity/alpha
          g/opacity/solid))
  (helper/opacity/set g/opacity/current))

;; theme ;;
(defun doom/colors (color)
  (cadr (assoc color doom-themes--colors)))

(use-package emacs
  :custom
  ;; ui
  (menu-bar-mode nil)         ;; Disable the menu bar
  (scroll-bar-mode nil)       ;; Disable the scroll bar
  (tool-bar-mode nil)         ;; Disable the tool bar

  ;; all the inhitibits
  (inhibit-splash-screen t)
  (inhibit-startup-screen t)
  (inhibit-startup-message t)
  (inhibit-startup-buffer-menu t)

  (blink-cursor-mode nil)     ;; Don't blink cursor

  ;; selection
  (use-short-answers t)       ;; Use y/n instead of yes/no
  (delete-selection-mode t)   ;; Select text and delete it by typing.
  (vc-follow-symlinks nil)    ;; follow git-symlink without prompt

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

  ;; history
  (savehist-mode) ;; Enables save history mode
  (make-backup-files nil) ;; Stop creating ~ backup files
  (auto-save-default nil) ;; Stop creating # auto save files

  :hook
  (before-save   . delete-trailing-whitespace)
  (prog-mode     . (lambda () (display-line-numbers-mode 1)))
  :config
  ;; Move customization variables to a separate file and load it, avoid filling up init.el with unnecessary variables
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  :bind (
         ([escape] . keyboard-escape-quit) ;; Makes Escape quit prompts (Minibuffer Escape)
         ;; Zooming In/Out
         ("C-=" . text-scale-increase)
         ("C--" . text-scale-decrease)
         ("<C-wheel-up>" . text-scale-increase)
         ("<C-wheel-down>" . text-scale-decrease)))

(defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (setq meow-use-clipboard t)
    ;; (meow-motion-define-key '("?" . ?)
    (meow-leader-define-key '("<tab>" . meow-comment))
    ;; (meow-normal-define-key
     ;; vim additions
     ;; ))
  (meow-motion-define-key
   '("k" . meow-next)
   '("j" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)
   ;; my additons
   '("%" . end-of-line)
   '("$" . beginning-of-line)
   '(":" . meow-M-x)
   '("`" . meow-cancel)

))

  (use-package meow)
  (meow-setup)
  (meow-global-mode 1)

(helper/mapkeys ctl-x-map
                '(("C-r"        . (lambda () (interactive) (load-file g/path/elispcfg)))
                  ("C-<return>" . helper/open-split-term)
                  ("C-b"        . ibuffer)))

(defvar emap/lsp (make-sparse-keymap))
(global-set-key (kbd "C-c l") emap/lsp)

(helper/mapkeys emap/lsp
                '(("f" . eglot-format-buffer)
                  ("r" . eglot-rename)
                  ("i" . imenu)))

(defvar emap/toggle (make-sparse-keymap))
(global-set-key (kbd "C-c t") emap/toggle)

(helper/mapkeys emap/toggle
                '(("b" . global-tab-line-mode)
                  ("n" . display-line-numbers-mode)
                  ("N" . global-display-line-numbers-mode)
                  ("o" . helper/opacity/toggle)
                  ("w" . visual-wrap-prefix-mode)
                  ("W" . global-visual-wrap-prefix-mode)))

(use-package dired
  :ensure nil ;; builtin
  :custom
  (dired-omit-files "^\\..*$") ;; hide . & .. folders
  :config
  (setq
   dired-listing-switches "-Aloh --group-directories-first"
   delete-by-moving-to-trash t
   ;; Dired don't create new buffer
   dired-kill-when-opening-new-dired-buffer t)
  ;; macos ls is poop, we need better ls
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
        (setq dired-use-ls-dired t
              insert-directory-program gls)))))

(use-package dired-collapse
  :hook (dired-mode . global-dired-collapse-mode))

(use-package diredfl :hook ((dired-mode . diredfl-mode)))

(if (executable-find "direnv") (use-package direnv))

(use-package vterm)

(use-package pdf-tools
  :init (pdf-loader-install))

(use-package doom-themes
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ;; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ;; if nil, italics is universally disabled
  :config
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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

(use-package projectile
  :custom
  (projectile-indexing-method 'alien)
  (projectile-run-use-comint-mode t)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-project-search-path '("~/Documents/" "~/Repos/")))

(use-package eglot
  :ensure nil
  :hook ((c-ts-mode
          c++-ts-mode
          haskell-mode
          java-mode
          lua-mode
          nix-ts-mode
          python-mode
          zig-mode)
         . eglot-ensure)
  :custom
  (eglot-sync-connect 0)  ;; async startup
  (eglot-autoshutdown t) ;; kill server when last buffer closes
  (eglot-events-buffer-size 0) ;; prevent huge debug buffers
  )

(use-package treesit-auto
  :after (tree-sitter)
  :custom
  (treesit-auto-install 't)
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

;; config/text
(use-package just-mode)
(use-package kdl-mode :mode "\\.kdl\\'")
(use-package ron-mode :mode "\\.ron\\'")

;; scripting/shell
(use-package fish-mode)
(use-package gnuplot-mode)
(use-package lua-mode)
(use-package nushell-mode)
(use-package nix-ts-mode :mode "\\.nix\\'")
(with-eval-after-load 'eglot
  (setq-default
   eglot-workspace-configuration
   '(:nixd
     (:formatting
      (:command ["alejandra"])))))

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :after (rust-mode)
  :config
  (setq rustic-format-on-save nil)
  (setq rustic-lsp-client 'eglot)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

(use-package org
  :custom
  (org-return-follows-link t)   ;; Sets RETURN key in org-mode to follow links
  :config
  (setq org-catch-invisible-edits 'show-and-error
        org-insert-heading-respect-content t
        ;; Org styling, hide markup etc.
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-replace-disputed-keys t
        ;; use [...] for folded text
        org-ellipsis " [...]"))

(set-face-attribute 'org-document-title nil :height 1.3 :weight 'bold)
(set-face-attribute 'org-document-info nil :height 1.2 :weight 'bold)
(set-face-attribute 'org-level-1 nil :height 1.2 :weight 'bold)
(set-face-attribute 'org-level-2 nil :height 1.2 :weight 'bold)
(set-face-attribute 'org-level-3 nil :height 1.1 :weight 'bold)

(use-package org-modern
  :init (global-org-modern-mode)
  :config
  (setq org-modern-star ["●" "○" "◆" "◇" "▶" "▷"]))

(setq org-latex-create-formula-image-program 'dvisvgm)
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-format-latex-options
    (plist-put org-format-latex-options :scale 1.5))

(defun helper/org/set-margins ()
  (let ((margin 2))
    (setq-local left-margin-width margin
                right-margin-width margin)
    (set-window-buffer nil (current-buffer))))
(add-hook 'org-mode-hook #'helper/org/set-margins)

;; have horizontal rule ignore magrins
(with-eval-after-load 'org-modern
  (set-face-attribute 'org-modern-horizontal-rule nil :extend t))

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
  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
  (add-to-list 'completion-at-point-functions #'cape-file) ;; Path completion
  (add-to-list 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
  (add-to-list 'completion-at-point-functions #'cape-keyword) ;; Keyword completion
  (add-to-list 'completion-at-point-functions #'cape-tex) ;; Complete Unicode char from TeX command, e.g. \hbar
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
  (completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion behaves like substring
  )

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

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
