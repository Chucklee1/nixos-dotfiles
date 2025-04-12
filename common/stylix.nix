{pkgs, ...}: {
  stylix = {
    enable = true;
    autoEnable = true;
    homeManagerIntegration.autoImport = true;
    image = ../assets/wallpaper.png;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/classic-dark.yaml";
    polarity = "dark";

    fonts = {
      monospace.package = pkgs.nerd-fonts.jetbrains-mono;
      monospace.name = "JetBrainsMono Nerd Font Mono";
      sansSerif.package = pkgs.noto-fonts-cjk-sans;
      sansSerif.name = "Noto Sans CJK";
      serif.package = pkgs.noto-fonts-cjk-serif;
      serif.name = "Noto Serif CJK";
    };
  };
}
