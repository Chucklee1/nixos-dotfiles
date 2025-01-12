{
  lib,
  pkgs,
  def,
  ...
}: {

  # general xserver options
  services.xserver = {
    enable = true;
    xkb.layout = def.layout;
    desktopManager.xterm.enable = false;
  };

  # dwm package
  services.xserver.windowManager.dwm = {
    enable = true;
    package = pkgs.dwm.overrideAttrs (oldAttrs: {src = /home/goat/dwm;});
  };

  # post dwm script
  systemd.user.services.PostScript = {
    description = "post run script after dwm";
    wantedBy = ["graphical-session.target"];
    partOf = ["graphical-session.target"];
    serviceConfig = {
      ExecStart = ''
        if xrandr | grep -q "DP-2"; then
          xrandr --output DP-2 --mode 1920x1080 -r 165.00
        else
          echo "DP-2 does not exist"
        fi
        ${lib.getExe pkgs.upower}
        ${lib.getExe pkgs.picom} --backend "egl" --vsync
        ${lib.getExe pkgs.redshift} -O 5200
        ${lib.getExe pkgs.dwm-status} /etc/dwm-status/config.toml
      '';
      RestartSec = 3;
      Restart = "always";
    };
  };

  # packages
  environment.systemPackages = with pkgs; [
    dmenu
    light
    xclip
  ];

  # desktop portal
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    config.common.default = ["gtk"];
  };

  # status bar config
  environment.etc."dwm-status/config.toml" = {
    text = ''
      order = ["audio", "backlight", "battery", "network", "time"]
      separator = "    "

      [audio]
      control = "Master"
      mute = ""
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
}
