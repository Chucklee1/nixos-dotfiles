{self, ...}: {
  home = [({pkgs, ...}: {
    services.mpd = let
      dir = "/srv/media";
    in {
      enable = true;
      musicDirectory = "${dir}/shared";
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
    home.packages = [pkgs.rmpc];
    home.file.".config/rmpc".source = "${self}/assets/rmpc";
  })];
}
