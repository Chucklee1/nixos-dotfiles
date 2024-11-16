{
  pkgs,
  lib,
  config,
  ...
}: {
  # -----------------------------------------------------------
  # System Packages & Programs Section
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # Wayland & Display Utilities
    wayland
    wayland-protocols
    wayland-utils
    wayland-scanner
    egl-wayland
    qt5.qtwayland
    qt6.qtwayland

    # Clipboard & Clipboard Management
    wl-clipboard
    cliphist
    xclip

    # Media Tools
    mpv
    imv
    ffmpeg
    v4l-utils

    # Keyboard & Input Tools
    wev
    ydotool
    wtype

    # System Controls
    playerctl
    pavucontrol
    brightnessctl
  ];

  # -----------------------------------------------------------
  # Home Packages Section
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

    # -----------------------------------------------------------
    # Home Programs Section (Startup Daemons)
    # -----------------------------------------------------------
    programs = {
      # Startup Daemons
      swww.enable = true;
      wlsunset.enable = true;
      dunst.enable = true;

      # Additional programs
      lazygit.enable = true;
      wlogout.enable = true;
      fuzzel.enable = true;
    };

    # -----------------------------------------------------------
    # Session Variables Section (Niri Configuration)
    # -----------------------------------------------------------
    sessionVariables = {
      DISPLAY = ":0"; # for Xwayland satellite
      XDG_CURRENT_DESKTOP = "niri";
      XDG_SESSION_DESKTOP = "niri";
      XDG_SESSION_TYPE = "wayland";
      GDK_BACKEND = "wayland";
      GTK_CSD = "0";
      CLUTTER_BACKEND = "wayland";
      QT_QPA_PLATFORM = "wayland";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      QT_AUTO_SCREEN_SCALE_FACTOR = "1";
      SDL_VIDEODRIVER = "wayland";
      MOZ_ENABLE_WAYLAND = "1";
      NIXOS_OZONE_WL = "1";
    };
  };

  # -----------------------------------------------------------
  # Security & Policy Section
  # -----------------------------------------------------------
  security = {
    rtkit.enable = true; # Enable RTKit for sound
    polkit.enable = true; # Enable PolicyKit
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
  # LXQt PolicyKit Agent Systemd Service Section
  # -----------------------------------------------------------
  systemd.user.services.lxqt-policykit-agent = {
    description = "LXQt PolicyKit Agent";
    serviceConfig.ExecStart = "${pkgs.lxqtPolicykitAgent}/bin/lxqt-policykit-agent";
    wantedBy = ["default.target"];
  };

  # -----------------------------------------------------------
  # XDG Desktop Portal Section
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
