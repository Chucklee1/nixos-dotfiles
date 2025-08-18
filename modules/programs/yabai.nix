{
  macbook.nix = [
    ({
      config,
      pkgs,
      ...
    }:
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
        services.spacebar = {
          enable = true;
          package = pkgs.spacebar;
          config = {
            # general
            position = "top";
            display = "main";
            height = bar_height;
            title = "on";
            spaces = "on";
            # clock
            clock = "on";
            clock_icon = "";
            clock_format = ''""%Y-%m-%d | %H:%M:%S""'';
            # power
            power = "on";
            power_icon_strip = " ";
            # dnd
            dnd_icon = "";
            # spaces
            space_icon = "•";
            space_icon_strip = "1 2 3 4 5 6 7 8 9 10";
            spaces_for_all_displays = "on";
            # right shell
            right_shell = "on";
            right_shell_icon = "";
            right_shell_command = "whoami";
            # padding
            padding_left = 0;
            padding_right = 0;
            spacing_left = 0;
            spacing_right = 0;
            # font
            text_font = ''"JetBrainsMono Nerd Font:Regular:12.0"'';
            icon_font = ''"Font Awesome 5 Free:Solid:12.0"'';
            # color
            background_color = "0xff${base00}";
            foreground_color = "0xff${base03}";
            power_icon_color = "0xff${base0A}";
            battery_icon_color = "0xff${base0A}";
            dnd_icon_color = "0xff${base03}";
            clock_icon_color = "0xff${base03}";
            space_icon_color = "0xff${base07}";
            space_icon_color_secondary = "0xff${base0C}";
            space_icon_color_tertiary = "0xff${base0A}";
          };
        };
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
  ];
}
