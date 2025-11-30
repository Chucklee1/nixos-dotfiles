{self, system, extlib, ...}: {
  nix = [
    (extlib.darwinOrLinux system
      {}
      ({pkgs, ...}: {
        environment.systemPackages = with pkgs; [udisks mpv pavucontrol];
        programs.dconf.enable = true;
      })
    )
  ];

  home = [
    {
      programs.zoxide = {
        enable = true;
        options = ["--cmd cd"];
      };
    }
    {
      programs.eza = {
        enable = true;
        enableZshIntegration = true;
        enableNushellIntegration = true;
      };
    }
    {
      programs.nix-your-shell = {
        enable = true;
        nix-output-monitor.enable = true;
      };
    }
  ];
}
