let
  nixLinux = {
    networking.firewall = {
      enable = true;
      allowedTCPPorts = [22 80 443];
    };
  };
in {
  nix.global = [{services.tailscale.enable = true;}];

  nix.desktop = [
    nixLinux
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
  nix.macbook = [
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

  home.global = [{services.syncthing.enable = true;}];

  home.desktop = [
    {
      services.mpd = let
        dir = "/media";
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
}
