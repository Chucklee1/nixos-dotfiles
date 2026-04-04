{self, ...}: let
  media_local_path = "/srv/media/Music";
  media_source = "nixos-inspiron:${media_local_path}";
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
    ({
      config,
      pkgs,
      user,
      ...
    }: {
      systemd.user.services.link-nfs-to-music = {
        description = "symlink local music to user Music Dir, override symlink if nfs marker is found";
        wantedBy = ["default.target"];
        after = ["mpd.service" "remote-fs.target"];
        script = ''
          TARGET="${config.users.users.${user}.home}/Music"
          MARKER="${media_path}/marker"

          # intially symlink local music
          ln -sfn ${media_local_path} "$TARGET"

          # override local dir is marker is found
          if [ -e "$MARKER" ]; then
            ln -sfn ${media_path} "$TARGET"
          fi

          # update mpd
          ${pkgs.mpc}/bin/mpc update
        '';
        serviceConfig.Type = "oneshot";
      };
    })
    # must be set at nix level for config to work here
    ({
      config,
      user,
      ...
    }: {
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
    ({
      config,
      pkgs,
      ...
    }: {
      # required mpd service
      services.mpd = {
        enable = true;
        musicDirectory = "${config.home.homeDirectory}/Music";
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
