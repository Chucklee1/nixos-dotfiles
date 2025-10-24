{
  home = [
    ({pkgs, ...}: {
      home.packages = with pkgs; [
        # calibre
        krita
        logisim-evolution
        # musescore
        picard
        qbittorrent
        tenacity
      ];
    })
  ];
}
