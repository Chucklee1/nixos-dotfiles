{
  nixos-global = [
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
      ];

      # env vars for x11
      environment.variables = {
        _JAVA_AWT_WM_NONREPARENTING = "1";
        XCURSOR_THEME = "Bibata-Modern-Classic";
        XCURSOR_SIZE = "24";
      };

      services = {
        xserver = {
          enable = true;
          xkb.layout = "us";
          desktopManager.xterm.enable = false;
          windowManager.dwm = {
            enable = true;
            package = pkgs.dwm.overrideAttrs (oldAttrs: {src = /home/goat/dwm;});
            extraSessionCommands = ''
              ${lib.getExe pkgs.feh} --bg-scale ${def.wallpaper}
              ${lib.getExe pkgs.redshift} -m randr -O 5200
            '';
          };
        };
        dwm-status = {
          enable = true;
          extraConfig = ''
            separator = "    "

            [audio]
            control = "Master"
            mute = "󰝟 X"
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
            template = "󰀂 {LocalIPv4} · {ESSID}"

            [time]
            format = " %Y-%m-%d  %H:%M:%S"
            update_seconds = true
          '';
        };
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
  ];

  home-global = [{services.gnome-keyring.enable = true;}];

  nixos-desktop = [
    {
      service = {
        dwm-status.order = ["audio" "network" "time"];
        displayManager.sessionCommands = "xrandr --output DP-2 --mode 1920x1080 --rate 165.00";
      };
    }
  ];

  nixos-laptop = [{service.dwm-status.order = ["audio" "battery" "backlight" "network" "time"];}];
}
