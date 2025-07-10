let
  root = "/media/goat/BLUE_SATA";
in
{
  nix.desktop = [
    # firewall
    {
      networking.firewall = {
        enable = true;
        allowedTCPPorts = [
          22
          80
        ];
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
    {
      services.navidrome = {
        enable = true;
        # port = 4533;
        settings = {
          EnableInsightsCollector = false;
          MusicFolder = "${root}/home/Music";
          DataFolder = "${root}/server/Navidrome/data";
          CacheFolder = "${root}/server/Navidrome/cache";
        };
      };
      services.audiobookshelf.enable = true;
      # port = 8000;
    }
  ];
  home.desktop = [
    # mpd
    {
      services.mpd = {
        enable = true;
        dataDir = "${root}/server/mpd";
        musicDirectory = "${root}/home/Music";
        playlistDirectory = "${root}/home/Music/[Playlist]";
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
