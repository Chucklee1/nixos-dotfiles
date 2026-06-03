{self, ...}: {
  nix = [
    # overlay
    ({
      config,
      pkgs,
      ...
    }: let
      emacs-pkg =
        if (config.services.xserver.enable)
        then pkgs.emacs
        else pkgs.emacs-pgtk;
    in {
      nixpkgs.overlays = [
        (import self.inputs.emacs-overlay)
        self.overlays.emacs
      ];
      environment.systemPackages = [emacs-pkg];
    })
  ];
}
