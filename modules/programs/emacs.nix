{self, ...}: {
  nix = [
    # overlay
    ({config, pkgs, ...}: let
      emacs-pkg =
        if pkgs.stdenv.isDarwin then pkgs.emacs-macport
        else if (config.services.xserver.enable) then pkgs.emacs
        else pkgs.emacs-pgtk;
    in {
      nixpkgs.overlays = [
        (import self.inputs.emacs-overlay)
        self.overlays.emacs
      ];
      # got to restate for mac to actually install emacs...
      environment.systemPackages =
        [emacs-pkg] ++
        (if pkgs.stdenv.isDarwin then [pkgs.coreutils-prefixed] else []);

      services.emacs.enable = true;
      services.emacs.package = emacs-pkg;
    })
  ];
}
