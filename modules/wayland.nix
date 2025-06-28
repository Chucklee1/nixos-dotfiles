{inputs, ...}: {
  nix.desktop = [
    inputs.niri.nixosModules.niri
    ({pkgs, ...}: {
      nixpkgs.overlays = [
        inputs.niri.overlays.niri
        (_: _: {waybar_git = inputs.waybar.packages.${pkgs.system}.waybar;})
      ];

      programs.niri = {
        enable = true;
        package = pkgs.niri-unstable;
      };

      environment = {
        variables = {
          XDG_CURRENT_DESKTOP = "niri";
          XDG_SESSION_DESKTOP = "niri";
          NIXOS_OZONE_WL = "1";
          MOZ_ENABLE_WAYLAND = "1";
          DISPLAY = ":0";
          _JAVA_AWT_WM_NONREPARENTING = "1";
          SDL_VIDEODRIVER = "x11";
          GDK_BACKEND = "wayland,x11";
          QT_QPA_PLATFORM = "wayland;xcb";
          QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
          QT_AUTO_SCREEN_SCALE_FACTOR = "1";
        };
        systemPackages = with pkgs; [
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
      };

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
