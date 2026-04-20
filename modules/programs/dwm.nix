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
          emacs --daemon &
          ${pkgs.feh}/bin/feh --bg-scale ${config.stylix.image}
        '';
      };


      systemd.user.services.slstatus = {
        description = "slstatus";
        wantedBy = [ "graphical-session.target" ];
        partOf = [ "graphical-session.target" ];
        serviceConfig.ExecStart = "${inputs.slstatus.packages.${pkgs.stdenv.hostPlatform.system}.default}/bin/slstatus";
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
          "80:class_g = 'Kitty'"
        ];
        settings = {
          blur.method = "dual_kawase";
        };
      };

      services.redshift = {
        enable = true;
        provider = "manual";
        temperature.day = 4800;
        temperature.night = 4800;
        # Baruunbayan-Ulaan, Övörkhangai, Mongolia
        settings.manual = {
          lat = 45.221;
          lon = 101.821;
        };
      };
    })
  ];
}
