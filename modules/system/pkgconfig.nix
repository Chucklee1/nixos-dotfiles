{inputs, ...}: {
  univ = [
    ({lib, pkgs, ...}: {
      nix.package = pkgs.nix;
      nix.settings.experimental-features = "nix-command flakes";
      nix.gc = {
        automatic = lib.mkDefault true;
        options = lib.mkDefault "--delete-older-than 7d";
      };
      nixpkgs.config.allowUnfree = true;
      nixpkgs.overlays = [inputs.chucks-package-repo.overlays.default];
    })
  ];
}
