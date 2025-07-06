let
  root = "/media/goat/BLUE_SATA/home/server";
in {
  nix.desktop = [
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
    ({pkgs, ...}: let
      # navidrome cfg
      settings = (pkgs.formats.json {}).generate "config.json" {
        EnableInsightsCollector = false;
        MusicFolder = "${root}/Media/Music";
        DataFolder = "${root}/Navidrome/data";
        CacheFolder = "${root}/Navidrome/cache";
      };
    in {
      systemd.user.services = {
        n-avidrome = {
          wantedBy = ["multi-user.target"];
          after = ["network.target"];
          serviceConfig.ExecStart = ''${pkgs.navidrome}/bin/navidrome --configfile ${settings}'';
        };
        a-udiobookshelf = {
          wantedBy = ["multi-user.target"];
          after = ["network.target"];
          serviceConfig.ExecStart = ''${pkgs.audiobookshelf}/bin/audiobookshelf --metadata ${root}/AudioBookshelf --config ${root}/AudioBookshelf'';
        };
      };
    })
  ];
  home.desktop = [
    # mpd
    {
      services.mpd = {
        enable = true;
        dataDir = "${root}/mpd";
        musicDirectory = "${root}/Media/Music";
        playlistDirectory = "${root}/Media/Music/[Playlist]";
        network.listenAddress = "any";
        extraConfig = ''
          save_absolute_paths_in_playlists "yes"
          audio_output {
            type "pipewire"
            name "MPDOUT"
          }
        '';
      };
    }
  ];
}
