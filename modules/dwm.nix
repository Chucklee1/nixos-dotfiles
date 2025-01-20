{
  lib,
  pkgs,
  def,
  ...
}: let
  dwmStatusConfig =
    if def.host == "laptop"
    then ''
      separator = "    "

      [audio]
      control = "Master"
      mute = "󰝟 MUTE"
      template = "{ICO} {VOL}%"
      icons = ["", "", ""]

      [backlight]
      device = "amdgpu_bl1"
      template = "{ICO} {BL}%"
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
    ''
    else ''
      separator = "    "

      [audio]
      control = "Master"
      mute = "󰝟 MUTE"
      template = "{ICO} {VOL}%"
      icons = ["", "", ""]

      [network]
      no_value = "󰯡"
      template = "󰀂 {LocalIPv4} · {ESSID}"

      [time]
      format = " %Y-%m-%d  %H:%M:%S"
      update_seconds = true
    '';

  dwmStatusOrder =
    if def.host == "laptop"
    then ["audio" "battery" "backlight" "network" "time"]
    else ["audio" "network" "time"];
in {
  # packages
  environment.systemPackages = with pkgs; [
    dmenu
    brightnessctl
    xclip
    picom
  ];

  services = {
    xserver = {
      enable = true;
      xkb.layout = "us";
      displayManager.sessionCommands =
        if def.host == "desktop"
        then "xrandr --output DP-2 --mode 1920x1080 --rate 165.00"
        else "";

      windowManager.dwm = {
        enable = true;
        package = pkgs.dwm.overrideAttrs {
          src = builtins.fetchGit {
            url = "https://github.com/Chucklee1/dwm";
            rev = "2af98f98610612660525ffa4a87cdc218f413b84";
          };
        };
        extraSessionCommands = ''
          export _JAVA_AWT_WM_NONREPARENTING=1
          export XDG_SESSION_TYPE=X11
          ${lib.getExe pkgs.feh} --bg-scale ${def.wallpaper}
          ${lib.getExe pkgs.redshift} -m randr -O 5200
        '';
      };
    };
    dwm-status = {
      enable = true;
      order = dwmStatusOrder;
      extraConfig = ''${pkgs.writeText "dwm-status.toml" ''${dwmStatusConfig}''}'';
    };
  };

  # desktop portal
  xdg.portal = {
    enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
      pkgs.xdg-desktop-portal-gnome
    ];
    config.common.default = "*";
  };
}
