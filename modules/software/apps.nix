{
  home = [
    ({pkgs, ...}: {
      home.packages = with pkgs; [
        calibre
        feishin
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
