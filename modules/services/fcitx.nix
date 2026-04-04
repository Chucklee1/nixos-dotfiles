{
  nix = [
    ({
      config,
      pkgs,
      ...
    }: {
      i18n.inputMethod = {
        enable = true;
        type = "fcitx5";
        fcitx5 = {
          addons = [pkgs.fcitx5-hangul];
          waylandFrontend = config.programs.niri.enable;
          # conflicts with emacs
          settings.addons.clipboard.globalSection.PastePrimaryKey = "";
        };
      };
    })
  ];
}
