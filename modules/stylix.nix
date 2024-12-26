{
  pkgs,
  lib,
  def,
  ...
}: {
  enable = true;
  autoEnable = true;
  homeManagerIntegration.autoImport = true;
  image = def.wallpaper;
  cursor.package = pkgs.bibata-cursors;
  cursor.name = "Bibata-Modern-Classic";
  cursor.size = 24;
  base16Scheme = "${pkgs.base16-schemes}/share/themes/classic-dark.yaml";

  fonts = {
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

  # system targets
  targets.grub.enable = false;
}
