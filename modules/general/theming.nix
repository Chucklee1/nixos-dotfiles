{
  pkgs,
  defaults,
  ...
}: {
  # -----------------------------------------------------------
  # stylix - main
  # -----------------------------------------------------------
  stylix = {
    enable = true;
    autoEnable = true;
    homeManagerIntegration.autoImport = true;
    image = defaults.wallpaper;
    cursor.package = pkgs.bibata-cursors;
    cursor.name = "Bibata-Modern-Classic";
    cursor.size = 24;
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
  # stylix - targets
  # -----------------------------------------------------------
  stylix.targets.grub.enable = false;
  home-manager.users.${defaults.username}.stylix.targets.waybar.enable = false;

  # -----------------------------------------------------------
  # boot theme
  # -----------------------------------------------------------
  boot.loader.grub2-theme = {
    enable = true;
    theme = "stylish";
    footer = true;
  };

  # -----------------------------------------------------------
  # home manager
  # -----------------------------------------------------------
  home-manager.sharedModules = [
    {
      home.packages = [
        pkgs.papirus-icon-theme
        (pkgs.nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
      ];
      gtk = {
        iconTheme = {
          name = "Papirus-Dark";
          package = pkgs.papirus-icon-theme;
        };
        gtk3.extraConfig = {
          gtk-application-prefer-dark-theme = 1;
        };
        gtk4.extraConfig = {
          gtk-application-prefer-dark-theme = 1;
        };
      };
      qt = {
        enable = true;
        style.name = "adwaita-dark";
        platformTheme.name = "gtk3";
      };
      programs.oh-my-posh = {
        enable = true;
        enableBashIntegration = true;
        useTheme = "pure";
      };
    }
  ];
}
