{
  gaming.nix = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        # emulation
        cemu
        joycond
        joycond-cemuhook
        ryubing
        # wine
        zenity
        wine
        wineWowPackages.stagingFull
        winetricks
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
        remotePlay.openFirewall = true;
        dedicatedServer.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
      };

      # roblox
      services.flatpak.enable = true;
      systemd.services.flatpak-repo = {
        enable = false;
        wantedBy = ["multi-user.target"];
        path = [pkgs.flatpak];
        script = ''flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo '';
      };
    })
  ];
  gaming.home = [{programs.mangohud.enable = true;}];
}
