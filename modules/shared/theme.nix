{pkgs, ...}: {
  # -----------------------------------------------------------
  # stylix - main
  # -----------------------------------------------------------
  stylix = {
    enable = true;
    autoEnable = true;
    homeManagerIntegration.autoImport = true;
    image = ../../assets/wallpaper.png;
    cursor.package = pkgs.bibata-cursors;
    cursor.name = "Bibata-Modern-Classic";
    cursor.size = 24;
    base16Scheme = {
      base00 = "#151515";
      base01 = "#202020";
      base02 = "#303030";
      base03 = "#505050";
      base04 = "#B0B0B0";
      base05 = "#D0D0D0";
      base06 = "#E0E0E0";
      base07 = "#F5F5F5";
      base08 = "#AC4142";
      base09 = "#D28445";
      base0A = "#F4BF75";
      base0B = "#90A959";
      base0C = "#75B5AA";
      base0D = "#6A9FB5";
      base0E = "#AA759F";
      base0F = "#8F5536";
    };
  };

  # -----------------------------------------------------------
  # stylix - font
  # -----------------------------------------------------------
  stylix.fonts = {
    monospace = {
      package = pkgs.nerd-fonts.jetbrains-mono;
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
