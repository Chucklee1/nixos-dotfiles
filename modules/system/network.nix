{
  nix = [
    ({lib, machine, ...}: {
      networking = {
        useDHCP = lib.mkDefault true;
        hostName = "nixos-${machine}";
        networkmanager.enable = true;
      };
      networking.firewall.enable = true;
    })
  ];
}
