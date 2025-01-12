{
  lib,
  pkgs,
  def,
  ...
}: let
  status-cfg = pkgs.writeText "dwm-status.toml" ''
    order = ["audio" "backlight" "battery" "cpu_load" "network" "time"];
    separator = "    "

    [audio]
    control = "Master"
    mute = ""
    template = "{ICO} {VOL}%"
    icons = []

    [backlight]
    device = "auto"
    template = "{ICO} {BL}%"
    icons = [󱩎, 󱩒, 󱩖]

    [battery]
    charging = "󱐋"
    discharging = "󰚦"
    enable_notifier = true
    no_battery = "󱉝"
    notifier_critical = 10
    notifier_levels = [2, 5, 10, 15, 20]
    separator = " · "
    icons = [, , , , ]

    [cpu_load]
    template = "{CL1} {CL5} {CL15}"
    update_interval = 20

    [network]
    no_value = "NA"
    template = "{LocalIPv4} · {ESSID}"

    [time]
    format = "%Y-%m-%d %H:%M"
    update_seconds = false
  '';
in {
  # overrides
  nixpkgs.overlays = [
    (self: super: {dwm = super.dwm.overrideAttrs (oldAttrs: {src = /home/goat/dwm;});})
  ];

  services = {
    xserver = {
      enable = true;
      xkb.layout = def.layout;
      desktopManager.xterm.enable = false;
      windowManager.session = [
        {
          name = "none+dwm";
          start = ''
            dwm &
            waitPID=$!

            if xrandr | grep -q "DP-2"; then
              xrandr --output DP-2 --mode 1920x1080 -r 165.00
            else
              echo "DP-2 does not exist"
            fi

            ${lib.getExe pkgs.feh} --bg-scale ${def.wallpaper}

            ${lib.getExe pkgs.picom} --backend "egl" --vsync

            ${lib.getExe pkgs.upower}
            ${lib.getExe pkgs.dwm-status} ${status-cfg}

            ${lib.getExe pkgs.redshift} -O 5200
          '';
        }
      ];
    };
  };

  # packages
  environment.systemPackages = with pkgs; [
    dwm
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
}
