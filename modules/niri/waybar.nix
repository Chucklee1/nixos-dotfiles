{inputs, ...}: let
  nixWaybar = [({system, ...}: {nixpkgs.overlays = [(_: _: {waybar_git = inputs.waybar.packages.${system}.waybar;})];})];
  homeWaybar = [
    ({
      lib,
      config,
      pkgs,
      system,
      ...
    }: {
      # waybar
      # stylix.targets.waybar.enable = false;
      programs.waybar = with config.lib.stylix.colors.withHashtag; let
        # helpers
        span = color: str: ''<span color="${color}" >${str}</span>'';
        red = base08;
        orange = base09;
        yellow = base0A;
        green = base0B;
        # ---- modules ----
        backlight = {
          format = ''{percent}% ${span yellow "{icon}"}'';
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
          format-warning = ''{capacity}% ${span orange "{icon}"}'';
          format-critical = ''{capacity}% ${span red "{icon}"}'';
          format-charging = ''{capacity}% ${span base0B "󱐋{icon}"}'';
          format-charging-warning = ''{capacity}% ${span orange "󱐋{icon}"}'';
          format-charging-critical = ''{capacity}% ${span red "󱐋{icon}"}'';
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
            locked = span red " ";
            unlocked = span base07 " ";
          };
        };
        mpd = {
          format = "{stateIcon}";
          format-disconnected = "󰝛";
          format-stopped = span red "";
          interval = 10;
          state-icons = {
            paused = "";
            playing = "";
          };
          tooltip = false;
        };
        network = {
          format-disconnected = "nah ⚠";
          format-ethernet = span base07 "{ipaddr}/{cidr}";
          format-wifi = span base07 "{essid} ";
          tooltip = false;
        };
        pulseaudio = {
          format = "{volume}% {icon}";
          format-bluetooth = "{volume}% {icon}";
          format-icons = {"default" = ["" "" ""];};
          format-muted = span red "M ";
          format-source = "{volume}% ";
          format-source-muted = span red "M ";
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
        enable = true;
        package = pkgs.waybar_git;
        systemd.enable = true;
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
in {
  nix.desktop = nixWaybar;
  home.desktop = homeWaybar;
}
