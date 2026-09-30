{self, ...}: {
  nix = [
    ({
      lib,
      pkgs,
      ...
    }: let
      emacs-pkg = pkgs.emacs-pgtk;
    in {
      nixpkgs.overlays = [
        (import self.inputs.emacs-overlay)
        self.overlays.emacs
      ];

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

      environment.sessionVariables.EDITOR = lib.mkForce "emacseditor";
    })
  ];

  # just in case
  home = [
    ({lib, ...}: {
      home.sessionVariables.EDITOR = lib.mkForce "emacseditor";
    })
  ];
}
