{inputs, ...}: {
  nix = [{nixpkgs.overlays = [inputs.quickshell.overlays.default];}];
  home = [
    ({config, pkgs, ...}: {
      programs.quickshell = {
        enable = true;
        systemd.enable = true;
        package = (pkgs.quickshell.withModules [
          inputs.qml-niri.packages.${pkgs.stdenv.hostPlatform.system}.default
        ]);
        activeConfig = "${config.home.homeDirectory}/Repos/nixos-dotfiles/assets/quickshell";
      };
    })
  ];
}
