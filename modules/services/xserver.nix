{
  nix = [
    {services.xserver.enable = true;}
  ];

  home = [
    ({config, pkgs, ...}: {
      home.packages = with pkgs; [
        dmenu
        xev
      ];

      xsession.enable = true;
      xsession.initExtra = ''
        ${pkgs.feh}/bin/feh --bg-scale ${config.stylix.image} &
      '';

      services.picom = {
        enable = true;
        vSync = true;
        backend = "glx";
        opacityRules = [
          "80:class_g = 'Emacs'"
          "80:class_g = 'Kitty'"
        ];
        settings = {
          blur.method = "dual_kawase";
        };
      };

      services.redshift = {
        enable = true;
        provider = "manual";
        temperature.day = 3800;
        temperature.night = 3800;
        # Baruunbayan-Ulaan, Övörkhangai, Mongolia
        settings.manual = {
          lat = 45.221;
          lon = 101.821;
        };
      };
    })
  ];
}
