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
    home-manager.users.goat = {
      home.packages = with pkgs; [
        vulkan-tools
        protonup
        mangohud
        wineWowPackages.waylandFull
        winetricks
        xwayland-satellite
      ];

      home.sessionVariables = {
        STEAM_EXTRA_COMPAT_TOOLS_PATHS = "\\\${HOME}/.steam/root/compatibilitytools.d";
      };
    };
  };
}
