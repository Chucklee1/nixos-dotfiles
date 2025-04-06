{
  nix.global = [
    {
      services.openssh = {
        enable = true;
        settings = {
          PasswordAuthentication = false;
          PermitRootLogin = "prohibit-password";
        };
      };
    }
  ];

  nix.laptop = [
    # tailscale
    {
      services.tailscale = {
        enable = true;
        port = 443;
        useRoutingFeatures = "server";
      };
    }
    # navidrome
    ({
      config,
      pkgs,
      ...
    }: let
      root = "/home/goat/Navidrome";

      settings = (pkgs.formats.json {}).generate "config.json" {
        Port = config.services.tailscale.port;

        EnableInsightsCollector = false;
        ArtistArtPriority = "external";

        MusicFolder = "/home/goat/music";
        DataFolder = "${root}/server";
        CacheFolder = "${root}/server/cache";
      };
    in {
      systemd.services.navidromee = {
        description = "Navidrome Media Server";
        after = ["network.target"];
        wantedBy = ["multi-user.target"];
        serviceConfig = {
          ExecStart = ''${pkgs.navidrome}/bin/navidrome --configfile ${settings}'';
          UMask = "0066";
        };
      };
    })
  ];
}
