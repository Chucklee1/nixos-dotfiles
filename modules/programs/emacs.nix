{
  emacs = {
    nix = [{services.emacs.enable = true;}];
    home = [
      ({pkgs, ...}: {
        programs.emacs = {
          enable = true;
          package = pkgs.emacsMacport;
          extraPackages = epkgs:
            with pkgs; [
              # vim clone
              epkgs.evil
              # auto-completion
              epkgs.company
              epkgs.company-auctex
              # tree sitter
              epkgs.tree-sitter
              epkgs.tree-sitter-langs
              # lsp
              epkgs.lsp-mode
              epkgs.lsp-ui
              # lang specific
              epkgs.nix-mode
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
              ;; Enable Evil
              (require 'evil)
              (evil-mode 1)

              ;; setup latex
              (setq TeX-view-program-selection '((output-pdf "Skim")))
              (setq TeX-source-correlate-method 'synctex)
              (setq TeX-source-correlate-start-server t)

              (use-package tree-sitter
                :hook ((nix-mode) . tree-sitter-mode)
                :config
                (use-package tree-sitter-langs))

              (use-package lsp-mode
                :commands lsp
                :hook ((nix-mode) . lsp)
                :config
                (use-package lsp-ui
                  :commands lsp-ui-mode))

              (use-package company
                :config
                (global-company-mode 1))
            '';
        };
      })
    ];
  };
}
