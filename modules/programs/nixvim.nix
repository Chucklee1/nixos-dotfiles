{self, ...}: {
  nix = [
    ({pkgs, ...}: {
      nixpkgs.overlays = [self.overlays.nixvim];
      environment.systemPackages = [pkgs.nixvim];
    })
  ];
}
