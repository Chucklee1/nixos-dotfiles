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
      services.avahi = {
        enable = true;
        nssmdns4 = true;
        openFirewall = true;
      };
    }
  ];
}
