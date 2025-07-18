let
  nixLinux = {
    networking.firewall = {
      enable = true;
      allowedTCPPorts = [22 80 443];
    };
  };
in {
  nix.global = [{tailscale.enable = true;}];

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

  home.global = [
    {
      services.syncthing.enable = true;
      services.mpd.enable = true;
    }
  ];

  home.desktop = [
    {
      services.mpd = let
        dir = "/media";
      in {
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
