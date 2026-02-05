{
  nix = [
    {
      services.libinput.enable = true;
      services.libinput.touchpad = {
        naturalScrolling = true;
      };

      services.xserver.enable = true;
    }
  ];

  home = [
    ({pkgs, ...}: {
      home.packages = with pkgs; [dmenu];

      services.picom = {
        enable = true;
        vSync = true;
        backend = "glx";
        opacityRules = ["80:class_g = 'Emacs'"];
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
