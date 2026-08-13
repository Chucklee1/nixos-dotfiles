{inputs, ...}: {
  nix = [
    ({pkgs, ...}: {
      nix.settings.substituters = ["https://attic.xuyh0120.win/lantian"];
      nix.settings.trusted-public-keys = ["lantian:EeAUQ+W+6r7EtwnmYjeVwx5kOGEBpjlBfPlzGlTNvHc="];
      nixpkgs.overlays = [inputs.nix-cachyos-kernel.overlays.pinned];
      boot.kernelPackages = pkgs.cachyosKernels.linuxPackages-cachyos-latest;
    })
  ];
}
