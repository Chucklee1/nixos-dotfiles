{
  home.desktop = [
    ({
      lib,
      config,
      pkgs,
      ...
    }: {
      # waybar
      # stylix.targets.waybar.enable = false;
      programs.waybar = with config.lib.stylix.colors.withHashtag; let
        # helpers
        span = color: str: ''<span color="${color}" >${str}</span>'';
        # ---- modules ----
        backlight = {
          format = ''{percent}% ${span base0A "{icon}"}'';
          format-icons = ["" "" "" "" "" "" "" "" ""];
        };
        battery = {
          interval = 30;
          states = {
            warning = 40;
            critical = 20;
          };
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
        clock = {
          format = "{:%F | %X}";
          interval = 1;
          tooltip = false;
        };
        idle_inhibitor = {
          format = "{icon}";
          format-icons = {
            activated = span base0C "";
            deactivated = "";
          };
          start-activated = true;
          tooltip = false;
        };
        keyboard-state = {
          numlock = true;
          capslock = true;
          format = {
            numlock = "N {icon}";
            capslock = "C {icon}";
          };
          format-icons = {
            locked = span base08 " ";
            unlocked = span base07 " ";
          };
        };
        mpd = {
          format = "{stateIcon}";
          format-disconnected = "󰝛";
          format-stopped = span base08 "";
          interval = 10;
          state-icons = {
            paused = "";
            playing = "";
          };
          tooltip = false;
        };
        network = {
          format-disconnected = span base0A "⚠";
          format-ethernet = span base0A "";
          format-wifi = span base07 "";
          tooltip = false;
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
        tray = {
          icon-size = 18;
          spacing = 15;
        };

        # ---- niri specific modules ----
        "niri/window" = {
          tooltip = false;
          format = "{}";
          max-length = 150;
        };
        "niri/workspaces" = {
          disable-scroll = true;
          disable-markup = true;
          disable-click = true;
          format = "{icon}";
          format-icons = {
            active = "";
            default = "";
          };
        };
      in {
        settings = [
          {
            position = "top";
            layer = "top";
            height = 24;

            inherit "niri/workspaces" "niri/window";
            modules-left = ["niri/workspaces" "niri/window"];

            inherit clock;
            modules-center = ["clock"];

            inherit keyboard-state mpd pulseaudio network backlight tray;
            modules-right = ["keyboard-state" "mpd" "pulseaudio" "network" "backlight" "battery" "tray"];
          }
        ];
        style =
          lib.mkAfter
          # css
          ''
            * {
              border: none;
              border-radius: 0;
            	font-family: JetBrainsMono Nerd Font;
            	font-size: 12px;
            	padding: 0;
            	margin: 0;
            }

            #waybar {
            	color: ${base05};
              background-color: alpha(${base00}, 0.8);
            }
            #workspaces button {
            	color: ${base05};
            }
            #window {
              padding-left: 2;
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
