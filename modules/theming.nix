{
  config,
  pkgs,
  def,
  ...
}: let 
files = import ./files.nix {inherit config;};
in{
  stylix = {
    enable = true;
    autoEnable = true;
    homeManagerIntegration.autoImport = true;
    image = def.wallpaper;
    base16Scheme = "${config.scheme}";

    cursor.package = pkgs.bibata-cursors;
    cursor.name = "Bibata-Modern-Classic";
    cursor.size = 24;

    fonts = {
      monospace.package = pkgs.nerd-fonts.jetbrains-mono;
      monospace.name = "JetBrainsMono Nerd Font Mono";
      sansSerif.package = pkgs.noto-fonts-cjk-sans;
      sansSerif.name = "Noto Sans CJK";
      serif.package = pkgs.noto-fonts-cjk-serif;
      serif.name = "Noto Serif CJK";

      sizes = {
        applications = 12;
        terminal = 12;
        desktop = 11;
        popups = 12;
      };
    };
  };

  home-manager.sharedModules = [
    {
      stylix = {
        iconTheme = {
          enable = true;
          package = pkgs.papirus-icon-theme;
          dark = "Papirus-Dark";
        };
        targets = {
          nixvim.transparentBackground.main = true;
          waybar.enable = false;
        };
      };
      gtk = {
        enable = true;
        gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;
        gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;
      };
      qt = {
        enable = true;
        style.name = "adwaita-dark";
        platformTheme.name = "gtk3";
      };
      programs.oh-my-posh =  {
        enable = true;
        settings = files.custom-pure;
      };
    }
  ];
}
