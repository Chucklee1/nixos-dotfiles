{
  nix = [{services.tailscale.enable = true;}];
  home = [({pkgs, ...}: {
      services.tailscale-systray.enable = pkgs.stdenv.isLinux;
    })  
  ];
}
