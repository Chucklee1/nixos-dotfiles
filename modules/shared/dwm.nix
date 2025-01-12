{
  pkgs,
  def,
  ...
}: {
  # overrides
  nixpkgs.overlays = [
    ()
    (self: super: {
      dwm = super.dwm.overrideAttrs (oldAttrs: {
        src = /home/goat/dwm;
      });
    })
    (self: super: {
      dwmblocks = super.dwmblocks.overrideAttrs (oldAttrs: {
        src = /home/goat/dwmblocks;
        nativeBuildInputs = [pkgs.pkg-config];
        buildInputs =
          super.dwmblocks.buildInputs
          ++ builtins.attrValues {inherit (pkgs.xorg) libxcb xcbutil;};
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
        name = "none+dwm";
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
          dwmblocks
        '';
      }
    ];
  };

  # system
  environment = {
    systemPackages = with pkgs; [
      dwm
      dwmblocks
      dmenu
      redshift
      light
      picom
      feh
      xclip
      nerd-fonts.symbols-only
      # bin scripts
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
