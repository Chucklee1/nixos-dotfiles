{
  nix = [({config, pkgs, ...}: {
    services.xserver.windowManager.bspwm = {
      enable = true;
      startupPrograms = [
        "${pkgs.feh}/bin/feh --bg-scale ${config.stylix.image} &"
      ];
    };
    services.sxhkd = with config.lib.stylix.colors.withhashtag; {
      enable = true;
      keybinds = let
        mod = "alt";
          # helpers
          dmenu = pkgs.writeShellScript "dmenu-run" ''
              dmenu-run -nb \
              "${base00}" \
              -nf "${base07}" \
              -sb "${base0d}" \
              -sf "${base00}"
            '';
      in {
        # programs
        "${mod} + Return" = "kitty";
        "${mod} + e" = "emacs";
        "${mod} + @space" = dmenu;
        # bspc
        "${mod} + {_,shift + }q" = "bspc node -{c,k}";
        "${mod} + ctrl + {q,r}" = "bspc {quit,wm -r}";
      };
    };
  })];
}
