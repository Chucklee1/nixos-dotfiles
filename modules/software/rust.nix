{inputs, ...}: {
  nix = [
    ({pkgs, ...}: {
      nixpkgs.overlays = [inputs.fenix.overlays.default];
      environment.systemPackages = with pkgs; [
        fenix.latest.toolchain
      ];
    })
  ];
}
