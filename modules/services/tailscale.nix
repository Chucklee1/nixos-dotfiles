{
  nix = [
    ({npkgs, ...}: {
      services.tailscale.enable = true;
      services.tailscale.package = npkgs.tailscale;
    })
  ];
}
