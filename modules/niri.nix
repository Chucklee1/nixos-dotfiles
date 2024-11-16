{
  pkgs,
  lib,
  config,
  ...
}: {
  imports = [niri-settings.nix];
  # system packages
  enviorment.systemPackages = with pkgs; [
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
  # home wm specific packages
  home-manager.users.goat.home.packages = with pkgs; [
    libnotify
    libsecret
    lxqt.lxqt-policykit
    swaylock-effects
    swayidle
  ];
  # system programs
  programs = {
    niri.enable = true;
    niri.package = pkgs.niri-unstable; # make niri use overlay poackage
    seahorse.enable = true; # password manager
  };
  # home programs
  home-manager.users.goat.programs = {
    # startup daemons
    swww.enable = true;
    wlsunset.enable = true;
    dunst.enable = true;
    # the rest
    lazygit.enable = true;
    wlogout.enable = true;
    fuzzel.enable = true;
  };
  # home manager specific stuff
  home-manager.users.goat = {
    # specify stylix targets
    stylix.targets.niri.enable = true;
    # niri variables
    home.sessionVariables = {
      DISPLAY = ":0"; # cool display thing for Xwayland satelite, idk what it rlly does
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
  # security section #
  security = {
    rtkit.enable = true; # sound
    polkit.enable = true; # polkit
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
          )
        {
          return polkit.Result.YES;
        }
      })
    '';
    pam.services.swaylock = {
      # locking
      text = ''
        auth include login
      '';
    };
  };

  # xdg portal
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
  # lxqt-policykit-agent
  systemd.user.services.lxqt-policykit-agent = {
    description = "LXQt PolicyKit Agent";
    serviceConfig.ExecStart = "${pkgs.lxqtPolicykitAgent}/bin/lxqt-policykit-agent";
    wantedBy = ["default.target"];
  };
}
