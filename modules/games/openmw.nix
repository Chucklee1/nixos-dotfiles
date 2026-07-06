{inputs, ...}: {
  nix = [
    ({pkgs, ...}: {
      nixpkgs.overlays = [
        inputs.chucks-package-repo.overlays.openmw
        inputs.chucks-package-repo.overlays.momw-tools-pack
      ];
      environment.systemPackages = [
        pkgs.openmw
        (pkgs.momw-tools-pack.override {generateFishCompletions = true;})
      ];
    })
  ];
}
