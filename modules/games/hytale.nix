{inputs, ...}: {
  nix = [
    ({pkgs, ...}: {
      nixpkgs.overlays = [inputs.hytale-launcher.overlays.default];
      environment.systemPackages = [pkgs.hytale-launcher];
    })
  ];
}
