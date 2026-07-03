{
  nix = [
    ({lib, ...}: {
      nix.settings.experimental-features = "nix-command flakes";
      nix.gc = {
        automatic = lib.mkDefault true;
        options = lib.mkDefault "--delete-older-than 7d";
      };
      nixpkgs.config.allowUnfree = true;
    })
  ];
}
