{
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        # emulation
        cemu
        joycond
        joycond-cemuhook
        ryubing
        # wine
        zenity
        jq
        # shader util
        vkbasalt
        # games
        osu-lazer-bin
        #prismlauncher
        openmw
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
        extraCompatPackages = [pkgs.proton-ge-bin];
        fontPackages = [pkgs.xlsfonts];
        remotePlay.openFirewall = true;
        dedicatedServer.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
      };
    })
  ];
  home = [
    {programs.mangohud.enable = true;}
    # Lazy shortcut for MO2
    ({
      config,
      pkgs,
      ...
    }: {
      home.packages = [
        (pkgs.writeShellScriptBin
          "MO2"
          ''
            protontricks-launch --appid 489830 \
            ${config.home.homeDirectory}/SSE/MO2/modorganizer2/ModOrganizer.exe
          '')
      ];
    })
  ];
}
