{
  nix = [
    (let
      music_dir = "/srv/media/Music";
    in
      {
        fileSystems."/srv/shared" = {
          device = "nixos-inspiron:${music_dir}";
          fsType = "nfs";
          options = [ "defaults" "auto" "_netdev" ];
        };
        services.navidrome = {
          enable = true;
          settings.Address = "localhost";
          settings.MusicFolder = music_dir;
        };
        services.audiobookshelf.enable = true; # port = 8000;
      })
  ];
}
