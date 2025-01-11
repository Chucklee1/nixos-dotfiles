{
  pkgs,
  def,
  ...
}: {
  # overrides
  nixpkgs.overlays = [
    (self: super: {
      dwm = super.dwm.overrideAttrs (oldAttrs: {
        src = /home/goat/dwm;
      });
    })
    (self: super: {
      slstatus = super.slstatus.overrideAttrs (oldAttrs: {
        src = /home/goat/slstatus;
      });
    })
  ];

  # X11
  services.xserver = {
    enable = true;
    xkb.layout = def.layout;
    desktopManager.xterm.enable = false;
    windowManager.session = [
      {
        name = "dmwmdmw";
        start = ''
          export _JAVA_AWT_WM_NONREPARENTING=1
          dwm &
          waitPID=$!

          if xrandr | grep -q "DP-2"; then
            xrandr --output DP-2 --mode 1920x1080 -r 165.00
          else
            echo "DP-2 does not exist"
          fi

          feh --bg-scale ${def.wallpaper}
          redshift -O 5200
          picom
          slstatus
        '';
      }
    ];
  };

  # system
  environment = {
    systemPackages = with pkgs; [
      dwm
      slstatus
      dmenu
      redshift
      acpilight
      picom
      feh
      xclip
      nerd-fonts.symbols-only
    ];
    variables = {
      _JAVA_AWT_WM_NONREPARENTING = 1;
    };
  };

  # desktop portal
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    config.common.default = ["gtk"];
  };
}
