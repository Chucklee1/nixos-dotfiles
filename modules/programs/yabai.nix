{
  macbook.nix = [
    ({config, ...}:
      with config.lib.stylix.colors; let
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
  ];
  macbook.home = [
    ({
      config,
      pkgs,
      ...
    }:
      with config.lib.stylix.colors; {
        home.packages = [
          (pkgs.writeShellScriptBin "sketchy-front_app" ''
            title=$(yabai -m query --windows --window | jq -r '.title')
            app=$(yabai -m query --windows --window | jq -r '.app')

            if [ -n "$title" ] && [ "$title" != "null" ]; then
              echo "  $title"
            else
              echo " $app"
            fi
          '')

          (pkgs.writeShellScriptBin "sketchy-keyboard" ''
            layout=$(defaults read ~/Library/Preferences/com.apple.HIToolbox.plist AppleSelectedInputSources | \
              grep -A1 'KeyboardLayout Name' | awk -F '"' '/KeyboardLayout Name/{getline; print $2}')

            if [ -z "$layout" ]; then
              layout="ENG"
            fi

            echo "⌨ $layout"
          '')

          (pkgs.writeShellScriptBin "sketchy-volume" ''
            volume=$(osascript -e "output volume of (get volume settings)")
            muted=$(osascript -e "output muted of (get volume settings)")

            if [ "$muted" = "true" ]; then
              echo "🔇 $volume%"
            else
              echo "🔊 $volume%"
            fi
          '')

          (pkgs.writeShellScriptBin "sketchy-network" ''
            ssid=$(/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I \
              | awk -F': ' '/ SSID/ {print $2}')

            if [ -n "$ssid" ]; then
              echo "📡 $ssid"
            else
              echo "⚠️ No Wi-Fi"
            fi
          '')

          (pkgs.writeShellScriptBin "sketchy-backlight" ''
            brightness=$(brightness -l | grep "brightness" | awk '{print int($NF*100)}')
            echo "☀️ $brightness%"
          '')

          (pkgs.writeShellScriptBin "sketchy-battery" ''
            percent=$(pmset -g batt | grep -Eo "[0-9]+%" | cut -d% -f1)
            status=$(pmset -g batt | grep 'Battery Power')

            if [[ $status == *"AC Power"* ]]; then
              echo "⚡ $percent%"
            else
              echo "🔋 $percent%"
            fi
          '')
        ];
        services.jankyborders = {
          enable = true;
          settings = {
            width = 3.0;
            style = "square";
            active_color = ''0xFF${base0D}'';
            inactive_color = ''0x00${base0D}'';
          };
        };
        programs.sketchybar = {
          enable = true;
          configType = "lua";
          config = ''
            -- ~/.config/sketchybar/sketchybarrc.lua

            local sbar = require("sketchybar")

            -- ============================================================
            -- LEFT
            -- ============================================================

            -- Yabai workspaces (spaces 1–10)
            for i = 1, 10 do
              sbar.add("space", "left", {
                associated_space = tostring(i),
                icon = { string = tostring(i), font = "SF Mono:Bold:14.0" },
                label = { drawing = false },
              })
            end

            -- Window title (from front_app script)
            sbar.add("item", "window_title", "left", {
              script = "sketchy-front_app",
              update_freq = 1,
              icon = { drawing = false },
              label = { font = "SF Pro Display:Regular:12.0", padding_left = 5 },
            })

            -- ============================================================
            -- CENTER
            -- ============================================================

            -- Date (left aligned in center block)
            sbar.add("item", "date", "center", {
              script = [[ date +"%m/%d/%Y" ]],
              update_freq = 60,
              icon = { drawing = false },
              label = { font = "SF Pro Display:Bold:12.0", align = "left" },
            })

            -- Time (right aligned in center block)
            sbar.add("item", "time", "center", {
              script = [[ date +"%H:%M:%S" ]],
              update_freq = 1,
              icon = { drawing = false },
              label = { font = "SF Pro Display:Bold:14.0", align = "right" },
            })

            -- ============================================================
            -- RIGHT
            -- ============================================================

            -- Keyboard state
            sbar.add("item", "keyboard", "right", {
              script = "sketchy-keyboard",
              update_freq = 2,
              icon = { drawing = true },
            })

            -- Audio
            sbar.add("item", "audio", "right", {
              script = "sketchy-volume",
              update_freq = 2,
              icon = { drawing = true },
            })

            -- Network
            sbar.add("item", "network", "right", {
              script = "sketchy-network",
              update_freq = 5,
              icon = { drawing = true },
            })

            -- Backlight
            sbar.add("item", "backlight", "right", {
              script = "sketchy-backlight",
              update_freq = 2,
              icon = { drawing = true },
            })

            -- Battery
            sbar.add("item", "battery", "right", {
              script = "sketchy-battery",
              update_freq = 30,
              icon = { drawing = true },
            })

            -- ============================================================
            -- BAR STYLE
            -- ============================================================

            sbar.config({
              padding_left = 5,
              padding_right = 5,
              height = 28,
              color = 0xff1e1e2e,   -- background color
              corner_radius = 8,
              blur_radius = 20,
            })
          '';
        };
      })
  ];
}
