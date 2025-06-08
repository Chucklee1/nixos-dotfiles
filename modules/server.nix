{
  nix.global = [
    # firewall
    {
      networking.firewall = {
        enable = true;
        allowedTCPPorts = [22 80];
      };
    }
    # ssh
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
  ];
  nix.desktop = [
    ({pkgs, ...}: let
      # folders
      MEDIA = "/media/goat/BLUE_SATA/home/server/Media";
      ND = "/home/goat/server/Navidrome";
      ABS = "/media/goat/BLUE_SATA/home/server/AudioBookshelf";

      # navidrome cfg
      settings = (pkgs.formats.json {}).generate "config.json" {
        EnableInsightsCollector = false;
        MusicFolder = "${MEDIA}/Music";
        DataFolder = "${ND}/data";
        CacheFolder = "${ND}/cache";
      };
    in {
      systemd.services = {
        n-avidrome = {
          wantedBy = ["multi-user.target"];
          after = ["network.target"];
          serviceConfig.ExecStart = ''${pkgs.navidrome}/bin/navidrome --configfile ${settings}'';
        };
        a-udiobookshelf = {
          wantedBy = ["multi-user.target"];
          after = ["network.target"];
          serviceConfig.ExecStart = ''${pkgs.audiobookshelf}/bin/audiobookshelf --metadata ${ABS} --config ${ABS}'';
        };
      };
    })
  ];
}
