{inputs, ...}: {
  nix = [
    ({config, pkgs, ...}: {
      services.xserver.enable = true;
      services.xserver.windowManager.dwm = {
        enable = true;
        package = pkgs.dwm.overrideAttrs {src = inputs.dwm;};
      };

      services.xserver.windowManager.dwm.extraSessionCommands = ''
        ${pkgs.feh}/bin/feh --bg-scale ${config.stylix.image} &
        ${inputs.slstatus.packages.${pkgs.stdenv.hostPlatform.system}.default}/bin/slstatus &
      '';
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
