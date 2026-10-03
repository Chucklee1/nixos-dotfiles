# will conflict with emacs.nix
# do not use both at same time
{self, inputs, ...}: {
  nix = [
    inputs.ewm.nixosModules.default
    ({config, pkgs, ...}: {
      nixpkgs.overlays = [
        (import self.inputs.emacs-overlay)
        inputs.ewm.overlays.default # for emacs31-pwayl
      ];
      programs.ewm = {
        enable = true;
        emacsPackage = pkgs.emacsWithPackagesFromUsePackage {
          package = pkgs.emacs31-pwayl;
          config = ''
            ,${builtins.readFile ../../pkgs/emacs/config.el}
          '';
          defaultInitFile = false;
          # make sure to include `(setq use-package-always-ensure t)` in config
          alwaysEnsure = true;
          # alwaysTangle = true;

          extraEmacsPackages = epkgs: [
            config.programs.ewm.ewmPackage
            epkgs.treesit-grammars.with-all-grammars
            pkgs.tree-sitter-grammars.tree-sitter-kdl
          ];
        };
      };
    })
  ];
}
