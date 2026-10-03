{
  self,
  inputs,
  ...
}: {
  nix = [
    ({
      config,
      pkgs,
      ...
    }: let
      emacs-pkg =
        if config.programs.ewm.enable
        then config.programs.ewm.emacsPackage
        else
          (pkgs.emacsWithPackagesFromUsePackage {
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
          });
    in {
      nixpkgs.overlays = [
        (import self.inputs.emacs-overlay)
        inputs.ewm.overlays.default # for emacs31-pwayl
      ];

      environment.systemPackages = [
        (pkgs.writeShellScriptBin "emacseditor" ''
          if [ -z "$1" ]; then
            exec ${emacs-pkg}/bin/emacsclient --create-frame --alternate-editor ${emacs-pkg}/bin/emacs
          else
            exec ${emacs-pkg}/bin/emacsclient --alternate-editor ${emacs-pkg}/bin/emacs "$@"
          fi
        '')
      ];
    })
  ];
}
