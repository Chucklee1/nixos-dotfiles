{
  pkgs,
  inputs,
  lib,
  config,
  ...
}: {
  options = {
    niri.enable = lib.mkEnableOption "enable niri window manager module";
  };

  config = lib.mkIf config.niri.enable {
    # -----------------------------------------------------------
    # niri - general settings
    # -----------------------------------------------------------
    nixpkgs.overlays = [inputs.niri.overlays.niri];
    programs.niri = {
      enable = true;
      package = pkgs.niri-unstable;
    };
    home-manager.users.goat = {
      # nested niri.settings so config.lib.niri.actions will work
      imports = [./settings.nix ./waybar.nix];
      # so I dont have to color borders, its nice
      stylix.targets.niri.enable = true;
    };
    # -----------------------------------------------------------
    # system - packaes
    # -----------------------------------------------------------
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
    ];

    # -----------------------------------------------------------
    # home - packages
    # -----------------------------------------------------------
    home-manager.users.goat.home.packages = with pkgs; [
      libnotify
      libsecret
      seahorse
      # in settings.nix startup
      swww
      dunst
      wlsunset
    ];

    # -----------------------------------------------------------
    # home - programs
    # -----------------------------------------------------------
    home-manager.users.goat.programs = {
      lazygit.enable = true;
      wlogout.enable = true;
      fuzzel.enable = true;
      swaylock.enable = true;
      swaylock.package = pkgs.swaylock-effects;
    };

    # -----------------------------------------------------------
    # system - security & policy
    # -----------------------------------------------------------
    services.gnome.gnome-keyring.enable = true;
    security = {
      rtkit.enable = true; # enable rtkit for sound
      polkit.enable = true; # enable policykit
      polkit.extraConfig = ''
        polkit.addRule(function(action, subject) {
          if (
            subject.isInGroup("users")
            && (
              action.id == "org.freedesktop.login1.reboot" ||
              action.id == "org.freedesktop.login1.reboot-multiple-sessions" ||
              action.id == "org.freedesktop.login1.power-off" ||
              action.id == "org.freedesktop.login1.power-off-multiple-sessions"
            )
          ) {
            return polkit.Result.YES;
          }
        })
      '';
      pam.services.swaylock = {
        text = ''
          auth include login
        '';
      };
    };

    # -----------------------------------------------------------
    # system - lxqt policykit agent
    # -----------------------------------------------------------
    systemd.user.services.lxqt-policykit-agent = {
      description = "LXQt PolicyKit Agent";
      serviceConfig.ExecStart = "${pkgs.lxqt.lxqt-policykit}/bin/lxqt-policykit-agent";
      wantedBy = ["default.target"];
    };

    # -----------------------------------------------------------
    # system - xdg desktop portal
    # -----------------------------------------------------------
    xdg.portal = {
      enable = true;
      extraPortals = [
        pkgs.xdg-desktop-portal-gtk
      ];
      configPackages = [
        pkgs.xdg-desktop-portal
        pkgs.xdg-desktop-portal-gtk
      ];
    };
  };
}