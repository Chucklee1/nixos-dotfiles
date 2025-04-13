{
  pkgs,
  inputs,
  ...
}: {
  # stylix
  imports = [inputs.stylix.darwinModules.stylix];

  stylix = {
    enable = true;
    autoEnable = true;
    homeManagerIntegration.autoImport = true;
    image = ../assets/wallpaper.png;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/classic-dark.yaml";
    polarity = "dark";

    fonts = {
      monospace.package = pkgs.nerd-fonts.jetbrains-mono;
      monospace.name = "JetBrainsMono Nerd Font Mono";
      sansSerif.package = pkgs.noto-fonts-cjk-sans;
      sansSerif.name = "Noto Sans CJK";
      serif.package = pkgs.noto-fonts-cjk-serif;
      serif.name = "Noto Serif CJK";
    };
  };

  # macso wm
  services = {
    yabai = {
      enable = true;
      config = {
        external_bar = "all:40:0";
        mouse_follows_focus = "off";
        focus_follows_mouse = "off";
        window_zoom_persist = "off";
        window_placement = "second_child";
        window_shadow = "float";
        window_opacity = "on";
        window_opacity_duration = 0.2;
        active_window_opacity = 1.0;
        normal_window_opacity = 0.8;
        window_animation_duration = 0.5;
        window_animation_easing = "ease_out_quint";
        insert_feedback_color = "0xff9dd274";
        split_ratio = 0.50;
        auto_balance = "off";
        auto_padding = "on";
        mouse_modifier = "fn";
        mouse_action1 = "move";
        mouse_action2 = " resize";
        mouse_drop_action = " swap";
        top_padding = 8;
        bottom_padding = 0;
        left_padding = 8;
        right_padding = 8;
        window_gap = 10;
      };
      extraConfig = ''
        yabai -m rule --add app="^(LuLu|Calculator|Software Update|Dictionary|VLC|System Preferences|System Settings|zoom.us|Photo Booth|Archive Utility|Python|LibreOffice|App Store|Steam|Alfred|Activity Monitor)$" manage=off
        yabai -m rule --add label="Finder" app="^Finder$" title="(Co(py|nnect)|Move|Info|Pref)" manage=off
        yabai -m rule --add label="Safari" app="^Safari$" title="^(General|(Tab|Password|Website|Extension)s|AutoFill|Se(arch|curity)|Privacy|Advance)$" manage=off
        yabai -m rule --add label="About This Mac" app="System Information" title="About This Mac" manage=off
        yabai -m rule --add label="Select file to save to" app="^Inkscape$" title="Select file to save to" manage=off

        yabai -m config layout bsp
      '';
    };
    sketchybar = {
      enable = true;
      config = ''
        # This is a demo config to showcase some of the most important commands.
        # It is meant to be changed and configured, as it is intentionally kept sparse.
        # For a (much) more advanced configuration example see my dotfiles:
        # https://github.com/FelixKratz/dotfiles

        PLUGIN_DIR="$CONFIG_DIR/plugins"

        ##### Bar Appearance #####
        # Configuring the general appearance of the bar.
        # These are only some of the options available. For all options see:
        # https://felixkratz.github.io/SketchyBar/config/bar
        # If you are looking for other colors, see the color picker:
        # https://felixkratz.github.io/SketchyBar/config/tricks#color-picker

        sketchybar --bar position=top height=40 blur_radius=30 color=0x40000000

        ##### Changing Defaults #####
        # We now change some default values, which are applied to all further items.
        # For a full list of all available item properties see:
        # https://felixkratz.github.io/SketchyBar/config/items

        default=(
          padding_left=5
          padding_right=5
          icon.font="Hack Nerd Font:Bold:17.0"
          label.font="Hack Nerd Font:Bold:14.0"
          icon.color=0xffffffff
          label.color=0xffffffff
          icon.padding_left=4
          icon.padding_right=4
          label.padding_left=4
          label.padding_right=4
        )
        sketchybar --default "$default[@]"

        ##### Adding Mission Control Space Indicators #####
        # Let's add some mission control spaces:
        # https://felixkratz.github.io/SketchyBar/config/components#space----associate-mission-control-spaces-with-an-item
        # to indicate active and available mission control spaces.

        SPACE_ICONS=("1" "2" "3" "4" "5" "6" "7" "8" "9" "10")
        for i in "$!SPACE_ICONS[@]"
        do
          sid="$(($i+1))"
          space=(
            space="$sid"
            icon="$SPACE_ICONS[i]"
            icon.padding_left=7
            icon.padding_right=7
            background.color=0x40ffffff
            background.corner_radius=5
            background.height=25
            label.drawing=off
            script="$PLUGIN_DIR/space.sh"
            click_script="yabai -m space --focus $sid"
          )
          sketchybar --add space space."$sid" left --set space."$sid" "$space[@]"
        done

        ##### Adding Left Items #####
        # We add some regular items to the left side of the bar, where
        # only the properties deviating from the current defaults need to be set

        sketchybar --add item chevron left \
                   --set chevron icon= label.drawing=off \
                   --add item front_app left \
                   --set front_app icon.drawing=off script="$PLUGIN_DIR/front_app.sh" \
                   --subscribe front_app front_app_switched

        ##### Adding Right Items #####
        # In the same way as the left items we can add items to the right side.
        # Additional position (e.g. center) are available, see:
        # https://felixkratz.github.io/SketchyBar/config/items#adding-items-to-sketchybar

        # Some items refresh on a fixed cycle, e.g. the clock runs its script once
        # every 10s. Other items respond to events they subscribe to, e.g. the
        # volume.sh script is only executed once an actual change in system audio
        # volume is registered. More info about the event system can be found here:
        # https://felixkratz.github.io/SketchyBar/config/events

        sketchybar --add item clock right \
                   --set clock update_freq=10 icon=  script="$PLUGIN_DIR/clock.sh" \
                   --add item volume right \
                   --set volume script="$PLUGIN_DIR/volume.sh" \
                   --subscribe volume volume_change \
                   --add item battery right \
                   --set battery update_freq=120 script="$PLUGIN_DIR/battery.sh" \
                   --subscribe battery system_woke power_source_change

        ##### Force all scripts to run the first time (never do this in a script) #####
        sketchybar --update
      '';
    };
    jankyborders.enable = true;
  };
}
