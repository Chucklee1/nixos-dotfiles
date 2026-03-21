{self, ...}: let
  media_source = "nixos-inspiron:/srv/media/Music";
  media_path = "/mnt/nfs";
in {
  nix = [
    {
      fileSystems.${media_path} = {
        device = media_source;
        fsType = "nfs";
        options = [
          "noatime"
          "_netdev"
          "x-systemd.automount"
          "x-systemd.device-timeout=10"
          "noauto"
        ];
      };
    }
    # must be set at nix level for config to work here
    ({config, user, ...}: {
      # scrobbing
      home-manager.users.${user} = {
        services.listenbrainz-mpd = {
          enable = true;
          settings.submission.token_file = config.sops.secrets."tokens/listenbrainz".path;
        };
      };
    })
  ];
  home = [
    ({config, pkgs, ...}: {
      # required mpd service
      services.mpd = {
        enable = true;
        musicDirectory = media_path;
        network.listenAddress = "any";
        extraConfig = ''
          audio_output {
            type "pipewire"
            name "MPDOUT"
          }
        '';
      };

      # global interaction service for mpd
      services.mpdris2 = {
        enable = true;
        multimediaKeys = true;
        # idk why but it works when I manually define here
        mpd.port = 6600;
        mpd.host = "127.0.0.1";
      };

      # rmpc
      home.packages = [pkgs.rmpc];
      home.file.".config/rmpc".source = "${self}/assets/rmpc";
    })
  ];
}
