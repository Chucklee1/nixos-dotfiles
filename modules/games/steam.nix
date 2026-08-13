{
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        file # for mo2-lint
        mo2-lint
        steam-run
        zenity
      ];
      programs.gamemode = {
        enable = true;
        settings.general.desiredgov = "performance";
        settings.general.renice = 10;
      };
      programs.steam = {
        enable = true;
        protontricks.enable = true;
        gamescopeSession.enable = true;
        remotePlay.openFirewall = true;
        dedicatedServer.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
      };
    })
  ];

  home = [
    {programs.mangohud.enable = true;}
  ];
}
