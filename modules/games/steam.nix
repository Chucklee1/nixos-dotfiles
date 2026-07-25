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
    # Nessecary stuff fhs-related tools
    ({
      pkgs,
      user,
      ...
    }: {
      services.envfs.enable = true;
      users.users.${user}.extraGroups = ["fuse"];
      programs.fuse.userAllowOther = true;

      programs.nix-ld.enable = true;
      programs.nix-ld.libraries = with pkgs; [
        libGL
        libGLX
        libX11
        libxkbcommon
        stdenv.cc.cc.lib # libstdc++
        wayland
      ];
    })
  ];

  home = [
    {home.sessionPath = ["$HOME/.local/bin"];}
    {programs.mangohud.enable = true;}
  ];
}
