{
  nix = [
    ({lib, ...}: {
      services.sunshine = {
        enable = true;
        capSysAdmin = true;
        openFirewall = true;
      };

      networking.firewall = {
        allowedTCPPorts = lib.mkAfter [ 47984 47989 47990 48010 ];
        allowedUDPPortRanges = lib.mkAfter [
          { from = 47998; to = 48000; }
          { from = 8000; to = 8010; }
        ];
      };
      # fix avahi stuff
      services.avahi.publish.enable = true;
      services.avahi.publish.userServices = true;
    })
  ];
}
