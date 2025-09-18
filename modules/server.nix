{
  additions.full.home = [{services.syncthing.enable = true;}];

  linux.home = [
    {
      services.mpd = let
        dir = "/srv/media";
      in {
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
    }
  ];

  metal.nix = [
    {services.tailscale.enable = true;}
    {
      networking.firewall = {
        enable = true;
        allowedTCPPorts = [22 80 443 8384];
      };
    }
  ];

  inspiron.nix = [
    {
      services.navidrome = {
        enable = true;
        settings.Address = "localhost";
        settings.MusicFolder = "/srv/media/Music";
      };
      services.audiobookshelf.enable = true; # port = 8000;
    }
  ];

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
