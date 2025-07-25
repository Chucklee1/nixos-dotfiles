let
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
in {
  global.nix = [{services.tailscale.enable = true;}];
  global.home = [{services.syncthing.enable = true;}];

  linux.nix = [
    {
      networking.firewall = {
        enable = true;
        allowedTCPPorts = [22 80 443];
      };
    }
  ];

  laptop.nix = [
    ({
      lib,
      pkgs,
      ...
    }: let
      mkJson = set: pkgs.writeText "navidrome-config.json" (lib.generators.toJSON {} set);
      navidromeCFG = let
        dir = "/srv";
      in
        mkJson {
          Address = "localhost";
          CacheFolder = "${dir}/navidrome/cache";
          DataFolder = "${dir}/navidrome/data";
          MusicFolder = "/media/Music";
          DefaultTheme = "Nord";
          CoverJpegQuality = "100";
        };
    in {
      systemd.services.set-perms = {
        description = "set user permissions folders outside of $HOME";
        wantedBy = ["multi-user.target"];
        before = ["navidrome.service"];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.writeShellScript "set perms" ''
            chown -R goat:media /media
            chmod -R 750 /media

          ''}";
        };
      };
      systemd.services.navidrome = {
        # port 4533
        enable = true;
        after = ["network.target"];
        wantedBy = ["default.target"];
        description = "Music-hosting service";
        serviceConfig.ExecStart = "${pkgs.navidrome}/bin/navidrome --configfile ${navidromeCFG}";
      };
      services.audiobookshelf.enable = true; # port = 8000;
    })
  ];
  desktop.home = [(mpd "/srv/media")];

  laptop.home = [(mpd "/media")];

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
