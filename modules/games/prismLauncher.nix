{inputs, ...}: {
  nix = [
    ({pkgs, ...}: {
      nixpkgs.overlays = [inputs.prismlauncher.overlays.default];

      environment.systemPackages = [
        pkgs.prismlauncher
      ];
    })
  ];
}
