{
  nix = [
    ({lib, machine, ...}: {
      system.stateVersion = "24.05";
      networking = {
        useDHCP = lib.mkDefault true;
        hostName = "nixos-${machine}";
        networkmanager.enable = true;
      };
      networking.firewall = {
        enable = true;
        allowedTCPPorts = [22 80 443 8384 8000];
      };
      i18n.defaultLocale = "en_CA.UTF-8";
      time.timeZone = "America/Vancouver";
    })
  ];
}
