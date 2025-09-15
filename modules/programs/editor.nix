{
  inputs,
  self,
  ...
}: let
  base = {
    nixpkgs.overlays = [inputs.nix-vim.overlays.default];
    environment.variables.EDITOR = "nvim";
  };
  coreDependants = {pkgs, ...}: {
    environment.systemPackages = with pkgs; [
      # lsps
      bash-language-server
      lua-language-server
      marksman
      nixd
    ];
  };
  fullDependants =
    coreDependants
    // ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        # lsps
        asm-lsp # GAS/GO assembly
        clang-tools
        jdtls # java
        lemminx # xml
        kdePackages.qtdeclarative
        typescript-language-server
        vscode-langservers-extracted
        # diagnostics
        statix
        # formatters
        alejandra
        html-tidy
        nodePackages.prettier
        shfmt
      ];
    });
in {
  umbra.nix = [
    base
    coreDependants
    ({pkgs, ...}: {
      environment.systemPackages = [pkgs.nixvim.core];
    })
  ];
  editor.nixvim.nix = [
    base
    fullDependants
    ({pkgs, ...}: {environment.systemPackages = [pkgs.nixvim.full];})
  ];

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
        };
      })
    ];
  };
}
