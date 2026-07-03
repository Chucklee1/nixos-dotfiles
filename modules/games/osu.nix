# mainstream nixpkgs lags behind a bit
{self, ...}: {
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = [(self.packages.${pkgs.stdenv.hostPlatform.system}.osu.override {nativeWayland = true;})];
    })
  ];
}
