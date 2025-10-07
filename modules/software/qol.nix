{self, extlib ...}: {
  nix = [
    (extlib.darwinOrLinux
      {}
      ({lib, pkgs, ...}: {
        environment.systemPackages = with pkgs; [udisks mpv pavucontrol];
        programs.dconf.enable = true;
      })
    )
  ];

  home = [
    ({pkgs, ...}: {
      home.packages = [pkgs.rmpc];
      home.file.".config/rmpc".source = "${self}/assets/rmpc";
    })
    {
      programs.zoxide = {
        enable = true;
        options = ["--cmd cd"];
      };
    }
  ];
}
