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
      };

      # rmpc
      home.packages = [pkgs.rmpc];
      home.file.".config/rmpc".source = "${self}/assets/rmpc";
    })
  ];
}
