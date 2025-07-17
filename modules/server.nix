let
  dir = "/media";
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
    ({
      lib,
      pkgs,
      ...
    }: let
      mkJson = set: pkgs.writeText "navidrome-config.json" (lib.generators.toJSON {} set);
      navidromeCFG = mkJson {
        MusicFolder = "${dir}/Music";
        DataFolder = "${dir}/navidrome/data";
        CacheFolder = "${dir}/navidrome/cache";
      };
    in {
      systemd.user.services.navidrome = {
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
  home.desktop = [
    {
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
    }
  ];
}
