let
  root = "/media/goat/BLUE_SATA/home/server";
in {
  nix.global = [
    # firewall
    {
      networking.firewall = {
        enable = true;
        allowedTCPPorts = [22 80];
      };
    }
    # mpd
    {
      services.mpd = {
        enable = true;
        dataDir = "${root}/mpd";
        musicDirectory = "${root}/Media/Music";
        network.listenAddress = "any";
        extraConfig = ''
          audio_output {
            type "pipewire"
            name "MPDOUT"
          }
        '';
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
    ({pkgs, ...}: {
      services.navidrome = {
        enable = true;
        settings = {
          EnableInsightsCollector = false;
          MusicFolder = "${root}Media/Music";
          DataFolder = "${root}/Navidrome/data";
          CacheFolder = "${root}/Navidrome/cache";
        };
      };
      systemd.services.a-udiobookshelf = {
        wantedBy = ["multi-user.target"];
        after = ["network.target"];
        serviceConfig.ExecStart = ''${pkgs.audiobookshelf}/bin/audiobookshelf --metadata ${root}/AudioBookshelf --config ${root}/AudioBookshelf'';
      };
    })
  ];
}
