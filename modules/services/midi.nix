{
  home = [
    ({pkgs, ...}: {
      home.packages = [
        pkgs.alsa-utils
      ];
      services.fluidsynth = let
        is_cfg = pkgs.writeText "is_fluid.txt" ''
          prog 0 0  # Yamaha Grand Piano
          prog 1 4  # Rhodes EP
          prog 2 6  # Harpsichord
          prog 3 7  # Clavinet
          prog 4 8  # Celesta
          prog 5 9  # Glockenspiel
          prog 6 19 # Church Organ
        '';
      in {
        enable = true;
        soundService = "pipewire-pulse";
        extraOptions = [
          "-o"
          "midi.autoconnect=1"
          "-m"
          "alsa_seq"
          "-f"
          "${is_cfg}"
        ];
      };
    })
  ];
}
