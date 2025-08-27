{self, ...}: let
  BAR_HEIGHT = "32";
in {
  macbook.nix = [
    {
      system.activationScripts.hammerspoon-setup.text = ''
        defaults write org.hammerspoon.Hammerspoon MJConfigFile "~/.config/hammerspoon/init.lua"
        sudo killall Dock
      '';
    }
  ];
  macbook.home = [
    {
      programs.aerospace.enable = true;
      programs.sketchybar.enable = true;
      programs.sketchybar.configType = "lua";
      programs.sketchybar.config = {
        source = "${self}/assets/sketchybar";
        recursive = true;
      };
    }
    ({pkgs, ...}:  {
      xdg.configFile."hammerspoon/init.lua".text = ''
          require("hs.ipc")
          local wmctl = "${pkgs.aerospace}/bin/aerospace"
          local mod = { "alt" }
          local modShift = { mod, "shift" }

          hs.hotkey.bind(mod, "return", function()
              hs.application.launchOrFocus("kitty")
          end)
          hs.hotkey.bind(mod, "space", function()
          hs.application.launchOrFocus("dmenu-mac")
          end)

          -- Fill screen: alt + m
          hs.hotkey.bind(mod, "m", function()
          hs.execute(winctl .. " fullscreen")
          end)

          -- Fullscreen: alt + shift + m
          hs.hotkey.bind(modShift, "m", function()
          hs.execute(winctl .. " macos-native-fullscreen")
          end)

          -- Focus window in direction: alt + arrow
          hs.hotkey.bind(mod, "up", function()
          hs.execute(winctl .. " focus up")
          end)
          hs.hotkey.bind(mod, "down", function()
          hs.execute(winctl .. " focus down")
          end)
          hs.hotkey.bind(mod, "left", function()
          hs.execute(winctl .. " focus left")
          end)
          hs.hotkey.bind(mod, "right", function()
          hs.execute(winctl .. " focus right")
          end)

          -- Move window in direction: alt + shift + arrow
          hs.hotkey.bind(modShift, "up", function()
          hs.execute(winctl .. " move up")
          end)
          hs.hotkey.bind(modShift, "down", function()
          hs.execute(winctl .. " move down")
          end)
          hs.hotkey.bind(modShift, "left", function()
          hs.execute(winctl .. " move left")
          end)
          hs.hotkey.bind(modShift, "right", function()
          hs.execute(winctl .. " move right")
          end)
        '';
      home.activation.reloadHammerspoon = ''
        $DRY_RUN_CMD /Applications/Hammerspoon.app/Contents/Frameworks/hs/hs -c "hs.reload()"
        $DRY_RUN_CMD sleep 1
        $DRY_RUN_CMD /Applications/Hammerspoon.app/Contents/Frameworks/hs/hs -c "hs.console.clearConsole()"
      '';
    })
  ];
}
