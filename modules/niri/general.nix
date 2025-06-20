{inputs, ...}: let
  environmentVariables = {
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
  nixNiri = [
    inputs.niri.nixosModules.niri
    ({pkgs, ...}: {
      nixpkgs.overlays = [inputs.niri.overlays.niri];

      programs.niri = {
        enable = true;
        package = pkgs.niri-unstable;
      };

      environment = {
        variables = environmentVariables;
        systemPackages = with pkgs; [
          egl-wayland
          qt5.qtwayland
          qt6.qtwayland
          brightnessctl
          wev
          wmenu
          xwayland
          xwayland-run
          wl-color-picker
          wl-clipboard
        ];
      };

      # polkit n portals
      security.polkit.enable = true;
      xdg.portal = {
        extraPortals = [pkgs.xdg-desktop-portal-gtk];
        config.common.default = "*";
      };
    })
  ];

  homeNiri = [
    ({
      lib,
      config,
      pkgs,
      ...
    }: {
      programs.swaylock = {
        enable = true;
        package = pkgs.swaylock-effects;
      };

      programs.niri.settings = {
        # general
        environment = environmentVariables;
        prefer-no-csd = true;
        hotkey-overlay.skip-at-startup = true;
        screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";

        spawn-at-startup = let
          get = pkg: lib.getExe pkgs.${pkg};
        in [
          {command = ["${get "xwayland-satellite"}"];}
          {command = ["${get "wlsunset"}" "-T" "5200"];}
          {command = ["${get "swaybg"}" "-m" "fill" "-i" "${config.stylix.image}"];}
          {command = ["brightnessctl" "s" "50%"];}
          {command = ["systemctl" "--user" "reset-failed" "waybar.service"];}
          {command = ["wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "1"];}
        ];
        # input
        input = {
          mouse.accel-speed = 0.0;
          tablet.map-to-output = "eDP-1";
          touch.map-to-output = "eDP-1";
          touchpad = {
            tap = true;
            dwt = true;
            natural-scroll = true;
            click-method = "clickfinger";
          };
        };
        # layout n theming
        layout = {
          gaps = 4;
          border.width = 2;
          always-center-single-column = false;
          tab-indicator = {
            hide-when-single-tab = true;
            place-within-column = true;
            width = 8.0;
          };
        };
        # disable annoying hot-corners
        gestures.hot-corners.enable = false;
        window-rules = let
          r = 4.0;
        in [
          {
            geometry-corner-radius = {
              top-left = r;
              top-right = r;
              bottom-left = r;
              bottom-right = r;
            };
            clip-to-geometry = true;
          }
          {
            matches = [{app-id = "^org.prismlauncher.PrismLauncher$";}];
            open-floating = false;
          }
        ];
      };
    })
  ];
in {
  nix.desktop = nixNiri;
  nix.laptop = nixNiri;
  home.desktop =
    homeNiri
    ++ [
      {
        programs.niri.settings.outputs."DP-1".mode = {
          width = 1920;
          height = 1080;
          refresh = 165.001;
        };
      }
    ];
}
