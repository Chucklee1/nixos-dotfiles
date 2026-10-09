{
  nix = [
    # ssh
    {
      services.openssh = {
        enable = true;
        settings.UseDns = true;
        # horrible idea but whatever
        settings.PasswordAuthentication = true;
      };
    }
    # dns resolving
    {
      services.resolved = {
        enable = true;
        settings.Resolve = {
          DNS = ["1.1.1.1" "1.0.0.1"];
          FallbackDNS = ["8.8.8.8" "8.8.4.4"];
          DNSSEC = "false";
        };
      };
    }
  ];
}
