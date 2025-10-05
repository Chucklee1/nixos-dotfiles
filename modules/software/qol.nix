{self, ...}: {
  nix = [
    ({lib, pkgs, ...}: lib.mkIf pkgs.stdenv.isLinux {
      environment.systemPackages = with pkgs; [udisks mpv pavucontrol];
      programs.dconf.enable = true;
    })
  ];

  home = [
    ({pkgs, ...}: {
      home.packages = [pkgs.rmpc];
      home.file.".config/rmpc".source = "${self}/assets/rmpc";
    })
    {
      zoxide = {
        enable = true;
        options = ["--cmd cd"];
      };
    }
  ];
}
