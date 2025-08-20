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
      system.activationScripts.hammerspoon-setup.text = ''
        defaults write org.hammerspoon.Hammerspoon MJConfigFile "~/.config/hammerspoon/init.lua"
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
    {
      xdg.configFile."hammerspoon/init.lua".text =
        #lua
        ''
          require("hs.ipc")

          hs.hotkey.bind({ "alt" }, "return", function()
              hs.application.launchOrFocus("kitty")
          end)
          hs.hotkey.bind({ "alt" }, "space", function()
            hs.application.launchOrFocus("dmenu-mac")

            -- Wait a short time to let the app open
            hs.timer.doAfter(0.5, function()
                local app = hs.appfinder.appFromName("dmenuMac")
                if app then
                    local win = app:mainWindow()
                    if win then
                        -- Bring the window above everything else
                        win:setLevel(hs.window.level.floating)
                    end
                end
            end)
          end)
        '';
      home.activation.reloadHammerspoon = ''
        $DRY_RUN_CMD /Applications/Hammerspoon.app/Contents/Frameworks/hs/hs -c "hs.reload()"
        $DRY_RUN_CMD sleep 1
        $DRY_RUN_CMD /Applications/Hammerspoon.app/Contents/Frameworks/hs/hs -c "hs.console.clearConsole()"
      '';
    }
  ];
}
