{
  pkgs,
  lib,
  config,
  ...
}: {
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
      prismlauncher
      vulkan-tools
      protonup-qt
      mangohud
      xwayland-satellite
    ];

    home.sessionVariables = {
      STEAM_EXTRA_COMPAT_TOOLS_PATHS = "\\\${HOME}/.steam/root/compatibilitytools.d";
    };
  };
}