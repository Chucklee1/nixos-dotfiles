{pkgs, ...}: {
  # -----------------------------------------------------------
  # home - variables
  # -----------------------------------------------------------
  home-manager.users.goat.home.sessionVariables = {
    DISPLAY = ":0";
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
    # dependencies
    libnotify
    libsecret
    # passsword app
    seahorse
    # wallpaper tool
    swayidle
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
  # home - services
  # -----------------------------------------------------------
  home-manager.users.goat.services = {
    wlsunset = {
      enable = true;
      latitude = "47.9522539697603"; # vertically relative
      longitude = "-122.2732338601"; # horizontally relative
    };
    dunst.enable = true;
    swayidle.enable = true;
  };

  # -----------------------------------------------------------
  # system - security & policy
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
  # ssytem - lxqt policykit agent
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
}
