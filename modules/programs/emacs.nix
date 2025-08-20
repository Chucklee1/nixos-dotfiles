{self, ...}: {
  emacs = {
    nix = [
      # overlay
      ({pkgs, ...}: {
        nixpkgs.overlays = [
          (import self.inputs.emacs-overlay)
          (self: super: {
            emacs-plus = pkgs.emacs-pgtk.overrideAttrs (old: {
              #patches =
              #(old.patches or [])
              #++ [
              ## Fix OS window role (yabai support)
              #(super.fetchpatch {
              #url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
              #sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
              #})
              #
              ## Rounded undecorated frame
              #(super.fetchpatch {
              #url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/round-undecorated-frame.patch";
              #sha256 = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
              #})
              #
              ## Dark/light mode awareness
              #(super.fetchpatch {
              #url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/system-appearance.patch";
              #sha256 = "sha256-oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8=";
              #})
              #];
            });
          })
        ];
      })
      {services.emacs.enable = true;}
    ];
    home = [
      ({pkgs, ...}: {
        programs.emacs = {
          enable = true;
          package = pkgs.emacs-plus;
          extraPackages = epkgs:
            with pkgs; [
              # vim clone
              epkgs.evil
              epkgs.evil-collection
              epkgs.doom-modeline
              epkgs.nerd-icons
              epkgs.nerd-icons-dired
              epkgs.nerd-icons-ibuffer
              epkgs.org
              epkgs.eat
              epkgs.auctex
              # lsps/other-related-ish
              nixd
              alejandra
              haskell-language-server
              lemminx # xml lsp
              stylua
              vscode-langservers-extracted # soyjack lsps
            ];
          extraConfig =
            #lisp
            ''
              (setq use-package-always-ensure nil) ;; let nix handle pkgs
              (setq gc-cons-threshold (* 50 1000 1000)) ;; reduce frequency of garbage collection

              (use-package emacs
                :custom
                (menu-bar-mode nil)         ;; Disable the menu bar
                (scroll-bar-mode nil)       ;; Disable the scroll bar
                (tool-bar-mode nil)         ;; Disable the tool bar
                (inhibit-startup-screen t)  ;; Disable welcome screen

                (delete-selection-mode t)   ;; Select text and delete it by typing.
                (electric-indent-mode nil)  ;; Turn off the weird indenting that Emacs does by default.
                (electric-pair-mode t)      ;; Turns on automatic parens pairing

                (blink-cursor-mode nil)     ;; Don't blink cursor
                (global-auto-revert-mode t) ;; Automatically reload file and show changes if the file has changed

                (display-line-numbers-type 'relative) ;; Relative line numbers
                (global-display-line-numbers-mode t)  ;; Display line numbers

                (mouse-wheel-progressive-speed nil) ;; Disable progressive speed when scrolling
                (scroll-conservatively 10) ;; Smooth scrolling
                (scroll-margin 8)

                (tab-width 4)

                (make-backup-files nil) ;; Stop creating ~ backup files
                (auto-save-default nil) ;; Stop creating # auto save files
                :hook
                (prog-mode . (lambda () (hs-minor-mode t))) ;; Enable folding hide/show globally
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

                ;; transparency
                (add-to-list 'default-frame-alist '(alpha-background . 90))

                ;; font
                (set-face-attribute 'default nil
                              :font "JetBrains Mono"
                              :height 120
                              :weight 'medium)
                (add-to-list 'default-frame-alist '(font . "JetBrains Mono"))
                (setq-default line-spacing 0.12)

                ;; modeline
                (use-package doom-modeline
                  :custom
                  (doom-modeline-height 25) ;; Set modeline height
                  :hook (after-init . doom-modeline-mode))

                ;; nerd fonts
                (use-package nerd-icons
                  :if (display-graphic-p))

                (use-package nerd-icons-dired
                  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

                (use-package nerd-icons-ibuffer
                  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

                ;; org
                (use-package org
                  :ensure nil
                  :custom
                  (org-edit-src-content-indentation 4) ;; Set src block automatic indent to 4 instead of 2.
                  (org-return-follows-link t)   ;; Sets RETURN key in org-mode to follow links
                  :hook
                  (org-mode . org-indent-mode) ;; Indent text
                  ;; The following prevents <> from auto-pairing when electric-pair-mode is on.
                  ;; Otherwise, org-tempo is broken when you try to <s TAB...
                  ;;(org-mode . (lambda ()
                  ;;              (setq-local electric-pair-inhibit-predicate
                  ;;                          `(lambda (c)
                  ;;                             (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
                  )

                (use-package eat
                  :hook ('eshell-load-hook #'eat-eshell-mode))

                ;; eglot
                (use-package eglot
                  :ensure nil ;; Don't install eglot because it's now built-in
                  :hook ((c-mode c++-mode ;; Autostart lsp servers for a given mode
                                 lua-mode) ;; Lua-mode needs to be installed
                         . eglot-ensure)
                  :custom
                  ;; Good default
                  (eglot-events-buffer-size 0) ;; No event buffers (LSP server logs)
                  (eglot-autoshutdown t);; Shutdown unused servers.
                  (eglot-report-progress nil) ;; Disable LSP server logs (Don't show lsp messages at the bottom, java)
                  ;; Manual lsp servers
                  ;;:config
                  ;;(add-to-list 'eglot-server-programs
                  ;;             `(lua-mode . ("PATH_TO_THE_LSP_FOLDER/bin/lua-language-server" "-lsp"))) ;; Adds our lua lsp server to eglot's server list
                  )

                ;; Make gc pauses faster by decreasing the threshold.
                (setq gc-cons-threshold (* 2 1000 1000))
                ;; Increase the amount of data which Emacs reads from the process
                (setq read-process-output-max (* 1024 1024)) ;; 1mb
            '';
        };
      })
    ];
  };
}
