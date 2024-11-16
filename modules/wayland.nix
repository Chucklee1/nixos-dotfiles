{
  pkgs,
  lib,
  config,
  ...
}: {
  # -----------------------------------------------------------
  # system packages & programs
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

  home-manager.users.goat = {
    # -----------------------------------------------------------
    # home packages
    # -----------------------------------------------------------
    home.packages = with pkgs; [
      # dependencies
      libnotify
      libsecret
      # passsword app
      seahorse
      # wallpaper tool
      swayidle
    ];
    # -----------------------------------------------------------
    # home programs
    # -----------------------------------------------------------
    programs = {
      lazygit.enable = true;
      wlogout.enable = true;
      fuzzel.enable = true;
      swaylock.enable = true;
      swaylock.package = pkgs.swaylock-effects;
    };
    # -----------------------------------------------------------
    # home services
    # -----------------------------------------------------------
    services = {
      wlsunset = {
        enable = true;
        latitude = "47.9522539697603"; # vertically relative
        longitude = "-122.2732338601"; # horizontally relative
      };
      dunst.enable = true;
      swayidle.enable = true;
    };
  };
  # -----------------------------------------------------------
  # security & policy
  # -----------------------------------------------------------
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
  # lxqt policykit agent
  # -----------------------------------------------------------
  systemd.user.services.lxqt-policykit-agent = {
    description = "LXQt PolicyKit Agent";
    serviceConfig.ExecStart = "${pkgs.lxqt.lxqt-policykit}/bin/lxqt-policykit-agent";
    wantedBy = ["default.target"];
  };

  # -----------------------------------------------------------
  # xdg desktop portal
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
}
