{
  nix = [
    ({pkgs, ...}: {
      i18n.inputMethod = {
        enabled = "fcitx5";
        fcitx5.addons = with pkgs; [
          fcitx5-hangul
          fcitx5-nord
        ];
      };
    })
  ];
}
