{inputs, ...}: {
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = [(pkgs.osu.override {nativeWayland = true;})];
    })
  ];
}
