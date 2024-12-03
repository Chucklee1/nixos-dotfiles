{
  pkgs,
  imports,
  ...
}: {
  # -----------------------------------------------------------
  # system - theming
  # -----------------------------------------------------------
  boot.loader.grub2-theme = {
    enable = true;
    theme = "stylish";
    footer = true;
  };
  stylix = {
    enable = true;
    homeManagerIntegration.autoImport = true;
    image = ../pictures/night-ridgeline.jpg;
    #tokyo-city-terminal-dark.yaml
    base16Scheme = {
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
    opacity.terminal = 0.9;
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
    targets.grub.enable = false;
  };
}
