{inputs, ...}: {
  nix = [
    ({
      config,
      pkgs,
      ...
    }: {
      services.xserver.enable = true;
      services.xserver.windowManager.dwm = {
        enable = true;
        package = pkgs.dwm.overrideAttrs {src = inputs.dwm;};
        extraSessionCommands = ''
          ${pkgs.feh}/bin/feh --bg-scale ${config.stylix.image} &
        '';
      };
    })
  ];

  home = [
    ({pkgs, ...}: {
      home.packages = with pkgs; [
        dmenu
        xev
      ];

      services.picom = {
        enable = true;
        vSync = true;
        backend = "glx";
        opacityRules = [
          "80:class_g = 'Emacs'"
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
