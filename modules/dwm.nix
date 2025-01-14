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
    dwm-status = {
      enable = true;
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
