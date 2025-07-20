let
  linux = {
    nix.networking.firewall = {
      enable = true;
      allowedTCPPorts = [22 80 443];
    };
    mpd = dir: {
      services.mpd = {
        enable = true;
        musicDirectory = "${dir}/Music";
        playlistDirectory = "${dir}/Music/playlist";
        network.listenAddress = "any";
        extraConfig = ''
          save_absolute_paths_in_playlists "yes"
          audio_output {
            type "pipewire"
            name "MPDOUT"
          }
        '';
      };
    };
  };
in {
  global = {
    nix = [{services.tailscale.enable = true;}];
    home = [{services.syncthing.enable = true;}];
  };

  desktop = {
    nix = [
      linux.nix
      ({
        lib,
        pkgs,
        ...
      }: let
        mkJson = set: pkgs.writeText "navidrome-config.json" (lib.generators.toJSON {} set);
        navidromeCFG = let
          dir = "/media";
        in
          mkJson {
            MusicFolder = "${dir}/Music";
            DataFolder = "${dir}/navidrome/data";
            CacheFolder = "${dir}/navidrome/cache";
            PlaylistsPath = "${dir}/Music/playlist";
            CoverJpegQuality = "100";
          };
      in {
        systemd.services.navidrome = {
          # port 4533
          enable = true;
          after = ["network.target"];
          wantedBy = ["default.target"];
          description = "Music-hosting service";
          serviceConfig.ExecStart = ''${pkgs.navidrome}/bin/navidrome --configfile ${navidromeCFG}'';
        };
        services.audiobookshelf.enable = true; # port = 8000;
      })
    ];
    home = [(linux.mpd "/media")];
  };

  laptop = {
    nix = [linux.nix];
    home = [(linux.mpd "/media")];
  };

  macbook.nix = [
    ({pkgs, ...}: {
      launchd.daemons.mpd = let
        mpdcfg = pkgs.writeText "mpd.conf" ''
          music_directory         "~/media/Music"
          playlist_directory      "~/media/Music/playlists"
          db_file                 "~/.mpd/mpd.db"
          log_file                "~/.mpd/mpd.log"
          pid_file                "~/.mpd/mpd.pid"
          state_file              "~/.mpd/mpdstate"
          auto_update             "yes"
          auto_update_depth       "2"
          follow_outside_symlinks "yes"
          follow_inside_symlinks  "yes"
          follow_outside_symlinks "yes"
          follow_inside_symlinks  "yes"

          audio_output {
              type                  "osx"
              name                  "CoreAudio"
              mixer_type            "software"
          }

          decoder {
              plugin                "mp4ff"
              enabled               "no"
          }
        '';
      in {
        script = ''
          mkdir -p /var/run/mpd /var/lib/mpd
          ${pkgs.mpd}/bin/mpd ${mpdcfg}
        '';
        serviceConfig = {
          KeepAlive = true;
          RunAtLoad = true;
        };
      };
    })
  ];
}
