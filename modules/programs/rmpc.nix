{self, ...}: {
  home = [
    ({pkgs, ...}: {
      # required mpd service
      services.mpd = {
        enable = true;
        musicDirectory = "/srv/shared";
        network.listenAddress = "any";
        extraConfig = ''
          audio_output {
            type "pipewire"
            name "MPDOUT"
          }
        '';
      };

      # global interaction service for mpd
      services.mpdris2 = {
        enable = true;
        multimediaKeys = true;
        # idk why but it works when I manually define here
        mpd.port = 6600;
        mpd.host = "127.0.0.1";
      };

      # scrobbing
      services.listenbrainz-mpd.enable = true;

      # rmpc
      home.packages = [pkgs.rmpc];
      home.file.".config/rmpc".source = "${self}/assets/rmpc";
    })
  ];
}
