{
  inputs,
  self,
  ...
}: {
  nix = [
    ({pkgs, ...}: {
      nixpkgs.overlays = [inputs.prismlauncher.overlays.default];

      environment.systemPackages = [
        pkgs.prismlauncher
        pkgs.temurin-bin-17
        pkgs.temurin-bin-25
        # it can be a pain so I just wrote a script
        (pkgs.writeShellScriptBin "getJDK" (builtins.readFile "${self}/assets/scripts/getJDK"))
      ];
    })
  ];
}
