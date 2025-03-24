{
  nix.laptop = [
    ({
      config,
      pkgs,
      ...
    }: let
      root = "/home/goat/navidrome";

      settings = (pkgs.formats.json {}).generate "d.json" {
        Address = "100.98.210.96";
        Port = config.services.tailscale.port;

        EnableInsightsCollector = false;
        EnableStarRating = false;

        MusicFolder = "${root}/music";
        DataFolder = "${root}/server";
        CacheFolder = "${root}/server/cache";

        LastFM = {
          Enabled = true;
          ApiKey = config.sops.templates."navi-lastfm-api-key".path;
          Secret = config.sops.templates."navi-lastfm-shared-secret".path;
        };
        Spotify = {
          ID = config.sops.templates."navi-spot-client-id".path;
          Secret = config.sops.templates."navi-spot-client-secret".path;
        };
        TLSCert = "${root}/server/laptop-nixos.monkey-court.ts.net.crt";
        TLSKey = "${root}/server/laptop-nixos.monkey-court.ts.net.key";
      };
    in {
      services = {
        tailscale = {
          enable = true;
          port = 3030;
          useRoutingFeatures = "server";
        };
      };

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
