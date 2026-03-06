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
    {
      # ssh
      services.openssh = {
        enable = true;
        # horrible idea but whatever
        settings.PasswordAuthentication = true;
      };
      # dns resolving
      services.avahi = {
        enable = true;
        nssmdns4 = true;
        openFirewall = true;
      };
    }
  ];
}
