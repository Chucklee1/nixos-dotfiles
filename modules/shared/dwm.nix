{
  lib,
  pkgs,
  def,
  ...
}: {
  # packages
  environment.systemPackages = with pkgs; [
    dmenu
    brightnessctl
    xclip
    # start script pkgs
    feh
    redshift
  ];

  services = {
    xserver = {
      enable = true;
      xkb.layout = def.layout;
      desktopManager.xterm.enable = false;
      windowManager.dwm = {
        enable = true;
        package = pkgs.dwm.overrideAttrs (oldAttrs: {src = /home/goat/dwm;});
        extraSessionCommands = ''
          if xrandr | grep -q "DP-2"; then
            xrandr --output DP-2 --mode 1920x1080 -r 165.00
          else
            echo "DP-2 does not exist"
          fi

          feh --bg-scale ${def.wallpaper}
          redshift -m randr -O 5200
        '';
        };
    };
    dwm-status = {
      enable = true;
      order = ["audio" "backlight" "battery" "network" "time"];
      extraConfig = ''
        separator = "    "

        [audio]
        control = "Master"
        mute = "󰝟"
        template = "{ICO} {VOL}%"
        icons = ["", "", ""]

        [backlight]
        device = "amdgpu_bl1"
        template = "{ICO} {BL}%"
        icons = ["󱩎", "󱩒", "󱩖"]

        [battery]
        charging = "󱐋"
        discharging = "󰚦"
        enable_notifier = true
        no_battery = "󱉝"
        notifier_critical = 10
        notifier_levels = [2, 5, 10, 15, 20]
        separator = " · "
        icons = ["", "", "", "", ""]

        [network]
        no_value = "󰯡"
        template = "{LocalIPv4} · {ESSID}"

        [time]
        format = "%Y-%m-%d %H:%M:%S"
        update_seconds = true
      '';
    };
    upower.enable = true;
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
