{
  nix = [
    # core network stuff
    ({
      lib,
      machine,
      ...
    }: {
      networking = {
        useDHCP = lib.mkDefault true;
        hostName = "nixos-${machine}";
        networkmanager.enable = true;
      };
      networking.firewall.enable = true;
    })
    # ntp timezone sync
    {services.ntp.enable = true;}
    # ssh
    {
      services.openssh = {
        enable = true;
        settings.UseDns = true;
        # horrible idea but whatever
        settings.PasswordAuthentication = true;
      };
    }
    # dns
    {
      services.avahi = {
        enable = true;
        nssmdns4 = true;
        openFirewall = true;
      };
    }
  ];
}
