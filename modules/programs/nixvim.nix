{self, ...}: {
  nix = [
    ({pkgs, ...}: {
      nixpkgs.overlays = [self.overlays.nixvim];
      environment.variables.EDITOR = "nvim";
      environment.systemPackages = [pkgs.nixvim];
    })
  ];
}
