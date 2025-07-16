let
  dir = "/srv";
in {
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
          MusicFolder = "${dir}/Music";
          PlaylistsPath = "${dir}/playlist";
          DataFolder = "${dir}/navidrome/data";
          CacheFolder = "${dir}/navidrome/cache";
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
        musicDirectory = dir;
        playlistDirectory = "${dir}/playlist";
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
