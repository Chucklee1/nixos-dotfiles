{
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [./hardware.nix];
  # -----------------------------------------------------------
  # software
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    protonup-qt
    protontricks
    prismlauncher
    osu-lazer-bin
  ];

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;
  };
  environment.variables.STEAM_EXTRA_COMPAT_TOOLS_PATHS = "~/.steam/root/compatibilitytools..d";

  services = {
    xserver.displayManager.sessionCommands = "${lib.getExe pkgs.xrog.xrandr} --output DP-2 --mode 1920x1080 -r 165.00";
  };
}
