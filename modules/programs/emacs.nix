{self, ...}: {
  nix = [
    ({
      config,
      pkgs,
      ...
    }: let
      emacs-pkg =
        if config.programs.ewm.enable
        then config.programs.ewm.emacsPackage
        else if pkgs.stdenv.hostPlatform.isDarwin
        then pkgs.callPackage ../../pkgs/emacs {emacsPackage = pkgs.emacs-macport;}
        else pkgs.callPackage ../../pkgs/emacs {emacsPackage = pkgs.emacs31-pgtk;};
    in {
      nixpkgs.overlays = [(import self.inputs.emacs-overlay)];

      environment.systemPackages = [
        emacs-pkg
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
