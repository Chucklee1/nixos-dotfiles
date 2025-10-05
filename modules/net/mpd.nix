{
  nix = [
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
}
