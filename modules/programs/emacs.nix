{self, ...}: {
  emacs = {
    nix = [
      # overlay
      {nixpkgs.overlays = [(import self.inputs.emacs-overlay)];}
      {services.emacs.enable = true;}
    ];
    home = [
      ({pkgs, ...}: {
        programs.emacs = {
          enable = true;
          package = pkgs.emacs-pgtk;
          extraPackages = epkgs:
            with pkgs; [
              nixd
              alejandra
              haskell-language-server
              lemminx # xml lsp
              stylua
              vscode-langservers-extracted # soyjack lsps
            ];
        };
      })
    ];
  };
}
