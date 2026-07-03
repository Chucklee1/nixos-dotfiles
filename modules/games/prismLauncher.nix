{inputs, ...}: {
  nix = [
    ({pkgs, ...}: {
      nix.settings = {
        trusted-substituters = [
          "https://prismlauncher.cachix.org"
        ];
        trusted-public-keys = [
          "prismlauncher.cachix.org-1:9/n/FGyABA2jLUVfY+DEp4hKds/rwO+SCOtbOkDzd+c="
        ];
      };

      nixpkgs.overlays = [inputs.prismlauncher.overlays.default];

      environment.systemPackages = [
        pkgs.prismlauncher
      ];
    })
  ];
}
