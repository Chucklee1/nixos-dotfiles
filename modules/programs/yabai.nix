{self, ...}: {
  macbook.nix = [
    (let
      bar_height = 26;
    in {
      services.yabai = {
        enable = true;
        enableScriptingAddition = true;
        config = {
          # general
          layout = "stack";
          external_bar = "all:0:${builtins.toString bar_height}";
          # gaps n padding
          window_gap = 0;
          top_padding = 0;
          bottom_padding = 0;
          left_padding = 0;
          right_padding = 0;
          # windows
          split_ratio = 0.5;
          auto_balance = "off";
        };
      };
    })
    ({user, ...}: {
      system.activationScripts.postUserActivation.text = ''
        defaults write org.hammerspoon.Hammerspoon MJConfigFile "/Users/${user}/.config/hammerspoon/init.lua"
        sudo killall Dock
      '';
    })
  ];
  macbook.home = [
    ({config, ...}:
      with config.lib.stylix.colors; {
        services.jankyborders = {
          enable = true;
          settings = {
            width = 3.0;
            style = "square";
            active_color = ''0xFF${base0D}'';
            inactive_color = ''0x00${base0D}'';
          };
        };
      })
    {
      home.file.".config/sketchybar".source = "${self}/assets/sketchybar";
      programs.sketchybar.enable = true;
    }
    ({config, ...}: {
      xdg.configFile."hammerspoon/".source = "${self}/assets/hammerspoon";
      home.activation.reloadHammerspoon =
        config.home-manager.users.${config.user}.lib.dag.entryAfter ["writeBoundary"]
        ''
          $DRY_RUN_CMD /Applications/Hammerspoon.app/Contents/Frameworks/hs/hs -c "hs.reload()"
          $DRY_RUN_CMD sleep 1
          $DRY_RUN_CMD /Applications/Hammerspoon.app/Contents/Frameworks/hs/hs -c "hs.console.clearConsole()"
        '';
    })
  ];
}
