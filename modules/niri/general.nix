{inputs, ...}: {
  nix.global = [
    inputs.niri.nixosModules.niri
    ({pkgs, ...}: {
      nixpkgs.overlays = [inputs.niri.overlays.niri];

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
        xwayland
        xwayland-run
        wl-color-picker
        wl-clipboard
      ];

      # polkit n portals
      security.polkit.enable = true;
      xdg.portal = {
        extraPortals = [pkgs.xdg-desktop-portal-gtk];
        config.common.default = "*";
      };
    })
  ];

  home.global = [
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

      programs.niri.settings = with config.lib.niri.actions; let
        sh = cmd: spawn "sh" "-c" "${cmd}";
        callExe = pkg: lib.getExe pkgs.${pkg};
      in {
        # general
        prefer-no-csd = true;
        hotkey-overlay.skip-at-startup = true;
        screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";

        spawn-at-startup = [
          {command = ["${callExe "xwayland-satellite"}"];}
          {command = ["${callExe "wlsunset"}" "-T" "5200"];}
          {command = ["${callExe "swaybg"}" "-m" "fill" "-i" "${config.stylix.image}"];}
          {command = ["brightnessctl" "s" "50%"];}
          {command = ["wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "1"];}
        ];

        switch-events = {
          tablet-mode-on.action = sh "notify-send tablet-mode-on";
          tablet-mode-off.action = sh "notify-send tablet-mode-off";
          lid-open.action = sh ''
            niri msg action power-on-monitors
            swaylock
          '';
          lid-close.action = sh "niri msg action power-off-monitors";
        };

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
        }; # layout n theming
        layout = {
          gaps = 4;
          border.width = 2;
          always-center-single-column = false;
          tab-indicator = {
            hide-when-single-tab = true;
            position = "top";
          };
        };
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

  home.desktop = [
    {
      programs.niri.settings.outputs."DP-1".mode = {
        width = 1920;
        height = 1080;
        refresh = 165.001;
      };
    }
  ];
}
