{
  pkgs,
  lib,
  def,
  ...
}: {
  stylix = {
    enable = true;
    autoEnable = true;
    homeManagerIntegration.autoImport = true;
    image = def.wallpaper;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/classic-dark.yaml";

    cursor.package = pkgs.bibata-cursors;
    cursor.name = "Bibata-Modern-Classic";
    cursor.size = 24;

    fonts.monospace.package = pkgs.nerd-fonts.jetbrains-mono;
    fonts.monospace.name = "JetBrainsMono Nerd Font Mono";

    fonts.sansSerif.package = pkgs.noto-fonts-cjk-sans;
    fonts.sansSerif.name = "Noto Sans CJK";

    fonts.serif.package = pkgs.noto-fonts-cjk-serif;
    fonts.serif.name = "Noto Serif CJK";

    fonts.sizes = {
      applications = 12;
      terminal = 12;
      desktop = 11;
      popups = 12;
    };

    targets.grub.enable = false;
  };

  home-manager.sharedModule = [
    {
      stylix.targets.waybar.enable = false;
      gtk = {
        enable = true;
        iconTheme.name = "Papirus-Dark";
        iconTheme.package = pkgs.papirus-icon-theme;
        gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;
        gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;
      };
      qt = {
        enable = true;
        style.name = "adwaita-dark";
        platformTheme.name = "gtk3";
      };
    }
  ];
}
