{
  config,
  lib,
  pkgs,
  ...
}: {
  options.games.enable = lib.mkEnableOption "enable gaming module";

  config = lib.mkIf config.games.enable {
    programs.gamescope.enable = true;
    programs.steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
      localNetworkGameTransfers.openFirewall = true;
    };
    environment.variables = {STEAM_EXTRA_COMPAT_TOOLS_PATHS = "~/.steam/root/compatibilitytools.d";};
    environment.systemPackages = with pkgs; [
      protonup-qt
      protontricks
      openmw
      prismlauncher
    ];
  };
}
