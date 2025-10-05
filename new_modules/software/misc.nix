{
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [udisks mpv pavucontrol];
      programs.dconf.enable = true;
    })
  ];
}
