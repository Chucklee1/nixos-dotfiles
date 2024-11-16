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

  # -----------------------------------------------------------
  # home packages
  # -----------------------------------------------------------
  home-manager.users.goat = {
    home.packages = with pkgs; [
      # dunst and related
      libnotify
      # security tools
      libsecret
      seahorse
      # swaylock and idle
      swaylock-effects
      swayidle
    ];
  };
  # -----------------------------------------------------------
  # home programs
  # -----------------------------------------------------------
  programs = {
    # startup daemons
    swww.enable = true;
    wlsunset.enable = true;
    dunst.enable = true;

    # additional programs
    lazygit.enable = true;
    wlogout.enable = true;
    fuzzel.enable = true;
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
    serviceConfig.ExecStart = "${pkgs.lxqtPolicykitAgent}/bin/lxqt-policykit-agent";
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
