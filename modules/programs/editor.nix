{
  inputs,
  self,
  ...
}: let
  base = {
    nixpkgs.overlays = [self.overlays.default];
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
  fullDependants = {pkgs, ...}: {
    environment.systemPackages = with pkgs; [
      # lsps
      asm-lsp # GAS/GO assembly
      clang-tools
      jdt-language-server # java
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
  };
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
    coreDependants
    fullDependants
    ({pkgs, ...}: {environment.systemPackages = [pkgs.nixvim.full];})
  ];

  editor.emacs = {
    nix = [
      # overlay
      ({machine, ...}: {
        nixpkgs.overlays = [
          (import self.inputs.emacs-overlay)
          (final: prev: {
            emacs-final = final.emacsWithPackagesFromUsePackage {
              package =
                if (machine != "inspiron")
                then final.emacs-pgtk
                else final.emacs;
              config = "${self}/assets/emacs/init.el";

              # substitution:
              #   defaultInitFile = pkgs.substituteAll {
              #     name = "default.el";
              #     src = ./emacs.el;
              #     inherit (config.xdg) configHome dataHome;
              #   };
              defaultInitFile = true;

              # make sure to include `(setq use-package-always-ensure t)` in config
              alwaysEnsure = true;
              # alwaysTangle = true;

              extraEmacsPackages = ep:
                with ep; [
                  (trivialBuild {
                    pname = "org-modern-indent";
                    version = "main";
                    src = inputs.org-modern-indent;
                  })
                  tree-sitter
                  treesit-grammars.with-all-grammars
                ];
            };
          })
        ];
      })
      ({pkgs, ...}: {
        services.emacs.enable = true;
        services.emacs.package = pkgs.emacs-final;
      })
    ];
    home = [

    ];
  };
}
