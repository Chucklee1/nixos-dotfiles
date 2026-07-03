{
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = [pkgs.zenity];
      programs.gamemode = {
        enable = true;
        settings.general.desiredgov = "performance";
        settings.general.renice = 10;
      };
      programs.steam = {
        enable = true;
        protontricks.enable = true;
        gamescopeSession.enable = true;
        extraCompatPackages = [pkgs.proton-ge-bin];
        fontPackages = [pkgs.xlsfonts];
        remotePlay.openFirewall = true;
        dedicatedServer.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
      };
    })
  ];

  home = [{programs.mangohud.enable = true;}];
}
