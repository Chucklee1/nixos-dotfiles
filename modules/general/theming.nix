{
  pkgs,
  wallpaper,
  ...
}: {
  # -----------------------------------------------------------
  # stylix - main
  # -----------------------------------------------------------
  stylix = {
    enable = true;
    autoEnable = true;
    homeManagerIntegration.autoImport = true;
    image = wallpaper;
    opacity.terminal = 0.7;
    cursor.package = pkgs.bibata-cursors;
    cursor.name = "Bibata-Modern-Classic";
    cursor.size = 24;
  };

  # -----------------------------------------------------------
  # base16
  # -----------------------------------------------------------
  # base16 theme: tokyo-city-terminal-dark.yaml
  stylix.base16Scheme = {
    base00 = "171D23";
    base01 = "1D252C";
    base02 = "28323A";
    base03 = "526270";
    base04 = "B7C5D3";
    base05 = "D8E2EC";
    base06 = "F6F6F8";
    base07 = "FBFBFD";
    base08 = "D95468";
    base09 = "FF9E64";
    base0A = "EBBF83";
    base0B = "8BD49C";
    base0C = "70E1E8";
    base0D = "539AFC";
    base0E = "B62D65";
    base0F = "DD9D82";
  };

  # -----------------------------------------------------------
  # stylix - targets
  # -----------------------------------------------------------
  stylix.targets = {
    grub.enable = false;
  };
  home-manager.users.goat.stylix.targets = {
    waybar.enable = false;
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
