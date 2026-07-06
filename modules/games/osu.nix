{inputs, ...}: {
  nix = [
    ({pkgs, ...}: {
      nixpkgs.overlays = [ inputs.chucks-package-repo.overlays.osu ];
      environment.systemPackages = [(pkgs.osu.override {nativeWayland = true;})];
    })
  ];
}
