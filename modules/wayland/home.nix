{
  imports,
  pkgs,
  ...
}: {
  stylix.targets = {
    niri.enable = true;
    waybar.enable = false;
  };
  home.packages = with pkgs; [
    # wm stuff
    libnotify
    libsecret
    seahorse
    papirus-icon-theme
    (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
    # in settings.nix startup
    swww
    dunst
    wlsunset
  ];

  programs = {
    fuzzel.enable = true;
    wlogout.enable = true;
    waybar = {
      enable = true;
      systemd.enable = true;
    };
  };
}
