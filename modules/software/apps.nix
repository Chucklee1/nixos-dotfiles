{
  home = [
    ({pkgs, ...}: {
      home.packages = with pkgs; [
        calibre
        krita
        logisim-evolution
        musescore
        muse-sounds-manager
        picard
        qbittorrent
        tenacity
      ];
    })
  ];
}
