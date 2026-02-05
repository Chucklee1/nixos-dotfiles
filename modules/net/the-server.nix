{
  nix = [
    (let
      music_dir = "/srv/media/Music";
    in {
      services.nfs.server.exports = ''
        ${music_dir} nixos-desktop(ro,fsid=0,no_subtree_check)
        ${music_dir} goat-macbook(ro,fsid=0,no_subtree_check)
      '';
      services.navidrome = {
        enable = true;
        settings.Address = "localhost";
        settings.MusicFolder = music_dir;
      };
      services.audiobookshelf.enable = true; # port = 8000;
    })
  ];
}
