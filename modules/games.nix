{
  nix.desktop = [
    # games
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        osu-lazer-bin
        prismlauncher
        ryubing
        cemu
        joycond
        joycond-cemuhook
      ];
      programs.steam = {
        enable = true;
        protontricks.enable = true;
        gamescopeSession.enable = true;
        extraCompatPackages = [pkgs.proton-ge-bin];
        remotePlay.openFirewall = true;
        dedicatedServer.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
      };
    })
    # wine
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        zenity
        samba
        wine
        wineWowPackages.stagingFull
        winetricks
      ];
    })
  ];
  # rip-off rivertuner
  home.desktop = [{programs.mangohud.enable = true;}];
}
