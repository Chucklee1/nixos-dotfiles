{self, ...}: {
  nix = [
    ({pkgs, machine, ...}: {
      nixpkgs.overlays = [self.overlays.default];
      environment.variables.EDITOR = "nvim";
      environment.systemPackages =
        (if machine == "umbra" then [pkgs.nixvim.core]
         else [pkgs.nixvim.full]);
    })
  ];
}
