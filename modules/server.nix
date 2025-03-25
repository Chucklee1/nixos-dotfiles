{inputs, ...}: let
  mkKey = secret: {
    sops.secrets."${secret}" = {owner = "goat";};
  };
in {
  nix.laptop = [
    # sops
    inputs.sops-nix.nixosModules.sops
    {
      sops = {
        defaultSopsFile = ../secrets.yaml;
        defaultSopsFormat = "yaml";
        age.keyFile = "home/goat/.config/sops/age/keys.txt";
      };
    }
    (mkKey "tailscale-auth-key")
    (mkKey "navi-lastfm-api-key")
    (mkKey "navi-lastfm-shared-secret")
    (mkKey "navi-spot-client-id")
    (mkKey "navi-spot-client-secret")

    # tailscale
    ({config, ...}: {
      services.tailscale = {
        enable = true;
        port = 3030;
        useRoutingFeatures = "server";
        authKeyFile = config.sops.secrets."tailscale-auth-key".path;
      };
    })
    # navidrome
    ({
      config,
      pkgs,
      ...
    }: let
      root = "/home/goat/navidrome";

      settings = (pkgs.formats.json {}).generate "config.json" {
        Address = "100.98.210.96";
        Port = config.services.tailscale.port;

        EnableExternalServices = true;
        EnableInsightsCollector = false;
        ArtistArtPriority = "external";

        MusicFolder = "${root}/music";
        DataFolder = "${root}/server";
        CacheFolder = "${root}/server/cache";
        TLSCert = "${root}/server/laptop-nixos.monkey-court.ts.net.crt";
        TLSKey = "${root}/server/laptop-nixos.monkey-court.ts.net.key";

        LastFM = {
          Enabled = true;
          ApiKey = config.sops.secrets."navi-lastfm-api-key".path;
          Secret = config.sops.secrets."navi-lastfm-shared-secret".path;
        };
        Spotify = {
          ID = config.sops.secrets."navi-spot-client-id".path;
          Secret = config.sops.secrets."navi-spot-client-secret".path;
        };
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
