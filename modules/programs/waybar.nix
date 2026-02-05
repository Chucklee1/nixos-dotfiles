{
  home = [
    ({
      lib,
      config,
      ...
    }: {
      # waybar
      stylix.targets.waybar.addCss = false;
      programs.waybar = with config.lib.stylix.colors.withHashtag; let
        merged = list: [(lib.mergeAttrsList list)];
        span = color: str: ''<span color="${color}" >${str}</span>'';
        wms = ["niri" "hyprland"];
      in {
        enable = true;
        systemd.enable = true;
        settings = merged [
          # general
          {
            position = "top";
            layer = "top";
            height = 24;
          }
          # left
          (lib.concatMapAttrs (wm: _: {
            modules-left = ["${wm}/workspaces" "${wm}/window"];
            "${wm}/window" = {
              tooltip = false;
              format = "{}";
              max-length = 150;
            };
            "${wm}/workspaces" = {
              disable-scroll = true;
              disable-markup = true;
              disable-click = true;
              format = "{icon}";
              format-icons.active = "";
              format-icons.default = "";
            };
          }) (lib.genAttrs wms (wm: wm)))

          # center
          {
            modules-center = ["clock"];
            clock = {
              format = "{:%F | %X}";
              interval = 1;
              tooltip = false;
            };
          }
          # right
          {
            modules-right = ["keyboard-state" "pulseaudio" "network" "backlight" "battery" "tray"];
            keyboard-state = {
              numlock = true;
              capslock = false;
              format.numlock = "N {icon}";
              format-icons.locked = span base08 " ";
              format-icons.unlocked = span base07 " ";
            };
            pulseaudio = {
              format = "{volume}% {icon}";
              format-bluetooth = "{volume}% {icon}";
              format-icons = {"default" = ["" "" ""];};
              format-muted = span base08 "";
              format-source = "{volume}% ";
              format-source-muted = span base08 "";
              tooltip = false;
            };
            network = {
              format-disconnected = span base0A "⚠";
              format-ethernet = span base0A "";
              format-wifi = span base07 "";
              tooltip = false;
            };
            backlight = {
              format = ''{percent}% ${span base0A "{icon}"}'';
              format-icons = ["" "" "" "" "" "" "" "" ""];
            };
            battery = {
              interval = 30;
              states.warning = 40;
              states.critical = 20;
              format-icons = [" " " " " " " " " "];
              format = ''{capacity}% ${span base0B "{icon}"}'';
              format-warning = ''{capacity}% ${span base09 "{icon}"}'';
              format-critical = ''{capacity}% ${span base08 "{icon}"}'';
              format-charging = ''{capacity}% ${span base0B "󱐋{icon}"}'';
              format-charging-warning = ''{capacity}% ${span base09 "󱐋{icon}"}'';
              format-charging-critical = ''{capacity}% ${span base08 "󱐋{icon}"}'';
              format-alt = "{icon} {time}";
              tooltip = false;
            };
            tray = {
              icon-size = 18;
              spacing = 15;
            };
          }
        ];
        style =
          # css
          ''
            * {
              border: none;
              border-radius: 0;
              font-family: JetBrainsMono Nerd Font;
              font-size: 14px;
              padding: 0;
              margin: 0;
            }

            window#waybar {
              background:  alpha(@base00, 0.8);
            }

            #window {
              padding-left: 10;
            }

            #mpd,
            #pulseaudio,
            #battery,
            #backlight,
            #network,
            #bluetooth,
            #idle_inhibitor,
            #tray {
              padding: 0 10 0 10
            }
          '';
      };
    })
  ];
}
