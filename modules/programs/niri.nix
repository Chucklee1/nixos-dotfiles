{
  inputs,
  pkgs,
  lib,
  config,
  ...
}: {
  options = {
    niri.enable = lib.mkEnableOption "enable niri window manager";
  };

  config = lib.mkIf config.niri.enable {
    # niri package
    nixpkgs.overlays = [inputs.niri.overlays.niri];
    programs.niri = {
      enable = true;
      package = pkgs.niri-unstable;
    };

    environment.systemPackages = with pkgs; [
      # wayland & display utilities
      wayland
      wayland-protocols
      wayland-utils
      wayland-scanner
      egl-wayland
      qt5.qtwayland
      qt6.qtwayland
      # clipboard & clipboard management
      wl-clipboard
      cliphist
      xclip
      # media tools
      mpv
      imv
      ffmpeg
      v4l-utils
      # keyboard & input tools
      wev
      ydotool
      wtype
      # system controls
      playerctl
      pavucontrol
      brightnessctl
      # wm stuff
      libnotify
      libsecret
      seahorse
      papirus-icon-theme
      (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
    ];

    services.gnome.gnome-keyring.enable = true;
    security = {
      rtkit.enable = true; # enable rtkit for sound
      polkit.enable = true; # enable policykit
    };

    xdg.portal = {
      enable = true;
      extraPortals = [pkgs.xdg-desktop-portal-gtk];
      configPackages = [
        pkgs.xdg-desktop-portal-gtk
        pkgs.xdg-desktop-portal
      ];
    };

    home-manager.sharedModules = [
      {
        # niri config packages and programs
        home.packages = with pkgs; [
          lxqt.lxqt-policykit
          dunst
          xwayland-satellite
          networkmanagerapplet
          swww
          wlsunset
        ];
        programs = {
          fuzzel.enable = true;
          wlogout.enable = true;
        };
        # niri config
        programs.niri.settings = {
          prefer-no-csd = true;
          hotkey-overlay.skip-at-startup = true;
          screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";

          environment = {
            NIXOS_OZONE_WL = "1";
            XDG_SESSION_TYPE = "wayland";
            XDG_CURRENT_DESKTOP = "niri";
            XDG_SESSION_DESKTOP = "niri";
            DISPLAY = ":0";
            GDK_BACKEND = "wayland";
            GTK_CSD = "0";
            CLUTTER_BACKEND = "wayland";
            QT_QPA_PLATFORM = "wayland;xcb";
            QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
            QT_AUTO_SCREEN_SCALE_FACTOR = "1";
            SDL_VIDEODRIVER = "x11";
            MOZ_ENABLE_WAYLAND = "1";
          };

          spawn-at-startup = [
            {command = ["lxqt-policykit"];}
            {command = ["dunst"];}
            {command = ["xwayland-satellite"];}
            {command = ["nm-applet"];}
            {command = ["wlsunset" "-t" "5000" "-T" "6500"];}
            {command = ["swww-daemon"];}
            {command = ["swww" "img" "/home/goat/nixos-dotfiles/Pictures/mono-forest.PNG"];}
            {command = ["waybar"];}
          ];

          input.keyboard.xkb.layout = "us";
          input.mouse.accel-speed = 1.0;
          input.touchpad = {
            tap = true;
            dwt = true;
            natural-scroll = true;
            click-method = "clickfinger";
          };
          input.tablet.map-to-output = "eDP-1";
          input.touch.map-to-output = "eDP-1";
          # borders n gaps
          layout = {
            gaps = 8;
            border.width = 2;
            always-center-single-column = false;
          };
          # corner rounding
          window-rules = [
            {
              matches = [];
              draw-border-with-background = false;
              geometry-corner-radius = {
                top-left = 12.0;
                top-right = 12.0;
                bottom-left = 12.0;
                bottom-right = 12.0;
              };
              clip-to-geometry = true;
            }
          ];

          outputs."DP-1" = {
            enable = true;
            mode.width = 1920;
            mode.height = 1080;
            position.x = 0;
            position.y = 0;
            mode.refresh = 165.001;
          };

          binds = {
            # programs
            "Mod+Return".action.spawn = ["kitty" "--working-directory" "~/nixos-dotfiles"];
            "Mod+Space".action.spawn = "fuzzel";
            "Super+Alt+L".action.spawn = "swaylock";
            "Super+Alt+P".action.spawn = "wlogout";

            # media keys
            "XF86AudioRaiseVolume".action.spawn = ["wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1+"];
            "XF86AudioLowerVolume".action.spawn = ["wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1-"];
            "XF86AudioMute".action.spawn = ["wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"];
            "XF86AudioMicMute".action.spawn = ["wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle"];
            "XF86MonBrightnessUp".action.spawn = ["brightnessctl" "--device=amdgpu_bl1" "s" "5%+"];
            "XF86MonBrightNessDown".action.spawn = ["brightnessctl" "--device=amdgpu_bl1" "s" "5%-"];

            # screenshot
            "Print".action.spawn = ["niri" "msg" "action" "screenshot"];
            "Ctrl+Print".action.spawn = ["niri" "msg" "action" "screenshot-screen"];
            "Alt+Print".action.spawn = ["niri" "msg" "action" "screenshot-window"];

            # window actions
            "Mod+Q".action.spawn = ["niri" "msg" "action" "close-window"];
            "Ctrl+Alt+Delete".action.spawn = ["niri" "msg" "action" "quit"];

            "Mod+Left".action.spawn = ["niri" "msg" "action" "focus-column-left"];
            "Mod+Right".action.spawn = ["niri" "msg" "action" "focus-column-right"];
            "Mod+Up".action.spawn = ["niri" "msg" "action" "focus-workspace-up"];
            "Mod+Down".action.spawn = ["niri" "msg" "action" "focus-workspace-down"];

            "Mod+Shift+Left".action.spawn = ["niri" "msg" "action" "move-column-left"];
            "Mod+Shift+Right".action.spawn = ["niri" "msg" "action" "move-column-right"];
            "Mod+Shift+Up".action.spawn = ["niri" "msg" "action" "move-window-to-workspace-up"];
            "Mod+Shift+Down".action.spawn = ["niri" "msg" "action" "move-window-to-workspace-down"];

            "Mod+Comma".action.spawn = ["niri" "msg" "action" "consume-window-into-column"];
            "Mod+Period".action.spawn = ["niri" "msg" "action" "expel-window-from-column"];

            "Mod+R".action.spawn = ["niri" "msg" "action" "switch-preset-column-width"];
            "Mod+M".action.spawn = ["niri" "msg" "action" "maximize-column"];
            "Mod+Shift+M".action.spawn = ["niri" "msg" "action" "fullscreen-window"];

            "Mod+Minus".action.spawn = ["niri" "msg" "action" "set-column-width" "-10%"];
            "Mod+Plus".action.spawn = ["niri" "msg" "action" "set-column-width" "+10%"];
            "Mod+Shift+Minus".action.spawn = ["niri" "msg" "action" "set-window-height" "-1%"];
            "Mod+Shift+Plus".action.spawn = ["niri" "msg" "action" "set-window-height" "+1%"];
          };
        };
      }
    ];
  };
}
