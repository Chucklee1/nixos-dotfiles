{self, extlib, ...}: {
  nix = [
    (extlib.darwinOrLinux
      {}
      ({pkgs, ...}: {
        environment.systemPackages = with pkgs; [udisks mpv pavucontrol];
        programs.dconf.enable = true;
      })
    )
  ];

  home = [
    (extlib.darwinOrLinux
      {}
      ({pkgs, ...}: {
        home.packages = [pkgs.rmpc];
        home.file.".config/rmpc".source = "${self}/assets/rmpc";
      })
    )
    {
      programs.zoxide = {
        enable = true;
        options = ["--cmd cd"];
      };
    }
  ];
}
