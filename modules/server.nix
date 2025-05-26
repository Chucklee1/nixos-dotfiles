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
    # tailscale
    {
      services.tailscale = {
        enable = true;
        port = 443;
        useRoutingFeatures = "server";
      };
    }
    # self-hosting
    ({
      config,
      pkgs,
      ...
    }: let
      MEDIA = "/media/goat/BLUE_SATA/home/server/Media";
      ND = "/goat/home/server/Navidrome";
      ABS = "/media/goat/BLUE_SATA/home/server/AudioBookshelf";

      settings = (pkgs.formats.json {}).generate "config.json" {
        Port = 100;

        EnableInsightsCollector = false;

        MusicFolder = "${MEDIA}/Music";
        DataFolder = "${ND}/data";
        CacheFolder = "${ND}/cache";
      };
    in {
      # navidrome
      systemd.services.navidromee = {
        description = "Navidrome Media Server";
        after = ["network.target"];
        wantedBy = ["multi-user.target"];
        serviceConfig = {
          ExecStart = ''${pkgs.navidrome}/bin/navidrome --configfile ${settings}'';
          UMask = "0066";
        };
      };
      # audiobookshelf
      systemd.services.audiobookshelff = {
        description = "AudioBookShelf audiobook server";
        after = ["network.target"];
        wantedBy = ["multi-user.target"];
        serviceConfig = {
          ExecStart = ''${pkgs.audiobookshelf}/bin/audiobookshelf --host 100.92.147.60 --port 200 --metadata ${ABS} --config ${ABS}'';
          UMask = "0066";
        };
      };
    })
  ];
}
