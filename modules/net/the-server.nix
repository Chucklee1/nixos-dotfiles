{
  nix = [
    {
      services.navidrome = {
        enable = true;
        settings.Address = "localhost";
        settings.MusicFolder = "/srv/media/Music";
      };
      services.audiobookshelf.enable = true; # port = 8000;
    }
  ];
}
