{inputs, ...}: {
  nix.desktop = [
    inputs.niri.nixosModules.niri
    ({pkgs, ...}: {
      nixpkgs.overlays = [
        inputs.niri.overlays.niri
        (final: _: {waybar_git = inputs.waybar.packages.${final.stdenv.hostPlatform.system}.waybar;})
      ];
      programs.niri = {
        enable = true;
        package = pkgs.niri-unstable;
      };

      environment.systemPackages = with pkgs; [
        egl-wayland
        qt5.qtwayland
        qt6.qtwayland
        brightnessctl
        wev
        wmenu
        swaynotificationcenter
        xwayland
        xwayland-run
        wl-color-picker
        wl-clipboard
      ];

      # polkit n portals
      security.polkit.enable = true;
      xdg.portal = {
        extraPortals = [
          pkgs.xdg-desktop-portal-gnome
          pkgs.xdg-desktop-portal-gtk
        ];
        config.common.default = "gnome";
      };
    })
  ];
}
