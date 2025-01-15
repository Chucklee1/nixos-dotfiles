{
  lib,
  pkgs,
  def,
  ...
}:
def.module "default" {
  # packages
  environment.systemPackages = with pkgs; [
    dmenu
    brightnessctl
    xclip
  ];

  services = {
    xserver = {
      enable = true;
      xkb.layout = "us";
      desktopManager.xterm.enable = false;
      windowManager.dwm = {
        enable = true;
        package = pkgs.dwm.overrideAttrs (oldAttrs: {src = /home/goat/dwm;});
        extraSessionCommands = ''
          export _JAVA_AWT_WM_NONREPARENTING=1
          xrandr --output DP-2 --mode 1920x1080 --rate 165.00
          ${lib.getExe pkgs.feh} --bg-scale ${def.wallpaper}
          ${lib.getExe pkgs.redshift} -m randr -O 5200
        '';
      };
    };
    dwm-status.enable = true;
    picom = {
      enable = true;
      backend = "egl";
      vSync = true;
    };
  };

  # desktop portal
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    config.common.default = ["gtk"];
  };
}
def.module "desktop" {
  services.dwm-status = {
    order = ["audio" "network" "time"];
    extraConfig = ''
      separator = "    "

      [audio]
      control = "Master"
      mute = "󰝟 X"
      template = "{ICO} {VOL}%"
      icons = ["", "", ""]

      [network]
      no_value = "󰯡"
      template = "󰀂 {LocalIPv4} · {ESSID}"

      [time]
      format = " %Y-%m-%d  %H:%M:%S"
      update_seconds = true
    '';
  };
}
def.module "laptop" {
  services.dwm-status = {
    order = ["audio" "battery" "backlight" "network" "time"];
    extraConfig = ''
      separator = "    "

      [audio]
      control = "Master"
      mute = "󰝟 MUTE"
      template = "{ICO} {VOL}%"
      icons = ["", "", ""]
      
      [backlight]
      device = "amd_bl1"
      template = "[ICO] {BL}%"
      icons = ["󱩎", "󱩒", "󱩖"]

      [battery]
      charging = ""
      discharging = ""
      enable_notifier = true
      notifier_critical = 10
      notifier_levels = [2, 5, 10, 15, 20]
      separator = " · "
      icons = ["", "", "", "", ""]

      [network]
      no_value = "󰯡"
      template = "󰀂 {LocalIPv4} · {ESSID}"

      [time]
      format = " %Y-%m-%d  %H:%M:%S"
      update_seconds = true
    '';
  };
}
