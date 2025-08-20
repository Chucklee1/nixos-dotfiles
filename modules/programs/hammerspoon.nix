{self, ...}: {
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
      services.yabai.enable = true;
      services.yabai.enableScriptingAddition = true;
      programs.sketchybar.enable = true;
      programs.sketchybar.configType= "lua";
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
