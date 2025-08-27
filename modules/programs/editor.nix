{inputs, self, ...}: let
  base = {nixpkgs.overlays = [inputs.nix-vim.overlays.default];};
in {
  umbra.nix = [base ({pkgs, ...}: {environment.systemPackages = [pkgs.nixvim.core];})];
  editor.nixvim.nix = [base ({pkgs, ...}: {environment.systemPackages = [pkgs.nixvim.full];})];
  editor.emacs = {
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
          extraPackages = epkgs: [
            epkgs.eat
            epkgs.org
            epkgs.tree-sitter-langs
          ];
        };
        home.packages = [
          pkgs.poppler-utils
          pkgs.texlive.combined.scheme-full
          pkgs.nixd
          pkgs.alejandra
          pkgs.haskell-language-server
          pkgs.lemminx # xml lsp
          pkgs.vscode-langservers-extracted # soyjack lsps
        ];
      })
    ];
  };
}
