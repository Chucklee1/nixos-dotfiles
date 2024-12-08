{
  lib,
  config,
  pkgs,
  ...
}: {
  config = lib.mkIf config.niri.enable {
    environment.systemPackages = with pkgs; [
      # wayland utilities
      wayland-utils
      wayland-scanner
      egl-wayland
      qt5.qtwayland
      qt6.qtwayland
      # clipboard management
      wl-clipboard
      cliphist
      # media tools
      mpv
      imv
      ffmpeg
      v4l-utils
      # I/O tools
      wev
      pavucontrol
      # libs
      libnotify
      libsecret
    ];

    services.gnome.gnome-keyring.enable = true;
    security = {
      rtkit.enable = true; # rtkit for sound
      polkit.enable = true;
    };

    home-manager.sharedModules = [
      {
        programs = {
          fuzzel.enable = true;
          wlogout.enable = true;
        };
      }
    ];
  };
}
