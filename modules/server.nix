let
  root = "/media/goat/BLUE_SATA/home/server";
  linuxNix = [
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
  ];
in {
  nix.laptop = linuxNix;
  nix.desktop =
    linuxNix
    ++ [
      ({pkgs, ...}: let
        # navidrome cfg
        settings = (pkgs.formats.json {}).generate "config.json" {
          EnableInsightsCollector = false;
          MusicFolder = "${root}/Media/Music";
          DataFolder = "${root}/Navidrome/data";
          CacheFolder = "${root}/Navidrome/cache";
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
            serviceConfig.ExecStart = ''${pkgs.audiobookshelf}/bin/audiobookshelf --metadata ${root}/AudioBookshelf --config ${root}/AudioBookshelf'';
          };
        };
      })
    ];
}
