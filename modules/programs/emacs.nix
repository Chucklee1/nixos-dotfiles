{self, ...}: {
  nix = [
    # overlay
    ({pkgs, ...}: let
      emacs-pkg =
        if pkgs.stdenv.isDarwin
        then pkgs.emacs-macport
        else pkgs.emacs-pgtk;
    in {
      nixpkgs.overlays = [
        (import self.inputs.emacs-overlay)
        self.overlays.emacs
      ];
      # got to restate for mac to actually install emacs...
      environment.systemPackages =
        [emacs-pkg]
        ++ (
          if pkgs.stdenv.isDarwin
          then [pkgs.emacs-macport pkgs.coreutils-prefixed]
          else []
        );
      envionment.variables.EDITOR = "emacseditor";
    })
  ];
}
