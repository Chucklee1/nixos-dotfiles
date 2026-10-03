{self, inputs, ...}: {
  nix = [
    ({pkgs, ...}: {
      nixpkgs.overlays = [
        (import self.inputs.emacs-overlay)
        inputs.ewm.overlays.default # for emacs31-pwayl
      ];

      services.emacs = {
        enable = true;
        package = pkgs.emacsWithPackagesFromUsePackage {
          package = pkgs.emacs31-pwayl;
          config = ''
            ,${builtins.readFile ../../pkgs/emacs/config.el}
          '';
          defaultInitFile = false;
          # make sure to include `(setq use-package-always-ensure t)` in config
          alwaysEnsure = true;
          # alwaysTangle = true;

          extraEmacsPackages = epkgs: [
            epkgs.treesit-grammars.with-all-grammars
            pkgs.tree-sitter-grammars.tree-sitter-kdl
          ];
        };
      };
    })
  ];

  # just in case
  home = [
    ({lib, ...}: {
      home.sessionVariables.EDITOR = lib.mkForce "emacseditor";
    })
  ];
}
