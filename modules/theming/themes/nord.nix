{self, ...}: {
  nix = [
    ({pkgs, ...}: {
      stylix = {
        image = "${self}/assets/wallpaper/nordest.png";
        base16Scheme = "${pkgs.base16-schemes}/share/themes/nord.yaml";
        polarity = "dark";
      };
    })
  ];
}
