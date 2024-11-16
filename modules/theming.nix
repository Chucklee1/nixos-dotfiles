{
  pkgs,
  lib,
  config,
  ...
}: {
  # -----------------------------------------------------------
  # theming
  # -----------------------------------------------------------
  fonts.packages = [(pkgs.nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})]; # needed for waybar and misc icons
  stylix = {
    enable = true;
    homeManagerIntegration.autoImport = true;
    image = ../wallpapers/clouds-sunset.jpg;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/tokyo-city-terminal-dark.yaml";
    opacity.terminal = 0.6;
    cursor.package = pkgs.bibata-cursors;
    cursor.name = "Bibata-Modern-Classic";
    cursor.size = 24;
    fonts = {
      monospace = {
        package = pkgs.nerdfonts.override {fonts = ["JetBrainsMono"];};
        name = "JetBrainsMono Nerd Font Mono";
      };
      sansSerif = {
        package = pkgs.noto-fonts-cjk-sans;
        name = "Noto Sans CJK";
      };
      serif = {
        package = pkgs.noto-fonts-cjk-serif;
        name = "Noto Serif CJK";
      };
      sizes = {
        applications = 12;
        terminal = 12;
        desktop = 11;
        popups = 12;
      };
    };
  };
  home-manager.users.goat = {
    # user theming
    gtk.iconTheme.name = "Papirus-Dark";
    gtk.iconTheme.package = pkgs.papirus-icon-theme;
  };
}
