{self, ...}: {
  home = [
    ({pkgs, ...}: {
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
      home.packages = [pkgs.rmpc];
      home.file.".config/rmpc".source = "${self}/assets/rmpc";
    })
  ];
}
