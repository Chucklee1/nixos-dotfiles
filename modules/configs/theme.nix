{
  pkgs,
  defaults,
  ...
}: {
  # -----------------------------------------------------------
  # stylix - main
  # -----------------------------------------------------------
  stylix = {
    autoEnable = true;
    homeManagerIntegration.autoImport = true;
    image = defaults.wallpaper;
    cursor.package = pkgs.bibata-cursors;
    cursor.name = "Bibata-Modern-Classic";
    cursor.size = 24;
    base16Scheme = {
      base00 = defaults.colors.base00;
      base01 = defaults.colors.base01;
      base02 = defaults.colors.base02;
      base03 = defaults.colors.base03;
      base04 = defaults.colors.base04;
      base05 = defaults.colors.base05;
      base06 = defaults.colors.base06;
      base07 = defaults.colors.base07;
      base08 = defaults.colors.base08;
      base09 = defaults.colors.base09;
      base0A = defaults.colors.base0A;
      base0B = defaults.colors.base0B;
      base0C = defaults.colors.base0C;
      base0D = defaults.colors.base0D;
      base0E = defaults.colors.base0E;
      base0F = defaults.colors.base0F;
    };
  };

  # -----------------------------------------------------------
  # stylix - font
  # -----------------------------------------------------------
  stylix.fonts = {
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

  # -----------------------------------------------------------
  # stylix targets ++ home manager - gtk/qt
  # -----------------------------------------------------------
  stylix.targets.grub.enable = false;

  home-manager.sharedModules = [
    {
      stylix.targets.waybar.enable = false;
      gtk = {
        iconTheme.name = "Papirus-Dark";
        iconTheme.package = pkgs.papirus-icon-theme;
        gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;
        gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;
      };
      qt = {
        style.name = "adwaita-dark";
        platformTheme.name = "gtk3";
      };
    }
  ];
}
