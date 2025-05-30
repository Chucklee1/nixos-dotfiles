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
  nix.desktop = [
    # tailscale
    {
      services.tailscale = {
        enable = true;
        port = 443;
        useRoutingFeatures = "server";
      };
    }
    # self-hosting
    ({pkgs, ...}: let
      MEDIA = "/media/goat/BLUE_SATA/home/server/Media";
      ND = "/home/goat/server/Navidrome";
      ABS = "/media/goat/BLUE_SATA/home/server/AudioBookshelf";

      settings = (pkgs.formats.json {}).generate "config.json" {
        Port = 100;

        EnableInsightsCollector = false;

        MusicFolder = "${MEDIA}/Music";
        DataFolder = "${ND}/data";
        CacheFolder = "${ND}/cache";
      };
    in {
      systemd.services = {
        # navidrome
        navidromee = {
          description = "Navidrome Media Server";
          after = ["network.target"];
          wantedBy = ["multi-user.target"];
          serviceConfig = {
            ExecStart = ''${pkgs.navidrome}/bin/navidrome --configfile ${settings}'';
            UMask = "0066";
          };
        };
        # audiobookshelf
        audiobookshelff = {
          description = "AudioBookShelf audiobook server";
          after = ["network.target"];
          wantedBy = ["multi-user.target"];
          serviceConfig = {
            # vpn ip, not doxing myself lol
            ExecStart = ''${pkgs.audiobookshelf}/bin/audiobookshelf --host 100.92.147.60 --port 200 --metadata ${ABS} --config ${ABS}'';
            UMask = "0066";
          };
        };
      };
    })
  ];
}
