{
  pkgs,
  lib,
  config,
  ...
}: {
  options = {
    steam.enable = lib.mkEnableOption "enable unlimited games";
  };

  config = lib.mkIf config.steam.enable {
    programs = {
      gamemode.enable = true;
      steam = {
        enable = true;
        gamescopeSession.enable = true;
        remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
        dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
        localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
      };
    };
    enviorment.systemPackages = [pkgs.grapejuice];
    home-manager.users.goat = {
      home.packages = with pkgs; [
        protonup
        mangohud
      ];

      home.sessionVariables = {
        STEAM_EXTRA_COMPAT_TOOLS_PATHS = "\\\${HOME}/.steam/root/compatibilitytools.d";
      };
    };
  };
}