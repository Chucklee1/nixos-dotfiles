{pkgs, ...}: {
  # waybar
  home.packages = [(pkgs.nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})];
  stylix.targets.waybar.enable = false;
  programs.waybar = {
    enable = true;
    settings = [
      {
        layer = "top";
        position = "top";
        height = 30;

        # ----- left modules ----- #
        modules-left = [
          "niri/workspaces"
          "custom/down-right-arrow"
        ];

        "niri/workspaces" = {
          format = "{icon}";
          "format-icons" = {
            focused = "󰻀";
            default = "";
          };
        };

        # ----- center modules ----- #
        modules-center = [
          "custom/up-left-arrow"
          "clock"
          "custom/down-right-arrow"
        ];

        clock = {
          format = "{ %H %M %m-%d}";
          tooltip = false;
        };

        # ----- right modules ----- #
        modules-right = [
          "custom/up-left-arrow"
          "pulseaudio"
          "memory"
          "cpu"
          "battery"
          "disk"
          "tray"
        ];

        # Pulseaudio settings
        pulseaudio = {
          format = "{icon} {volume:2}%";
          "format-bluetooth" = "{icon}  {volume}%";
          "format-muted" = "MUTE";
          "format-icons" = {
            headphones = "";
            default = [
              ""
              ""
            ];
          };
          "scroll-step" = 5;
          "on-click" = "pamixer -t";
          "on-click-right" = "pavucontrol";
        };

        memory = {
          interval = 5;
          format = "Mem {}%";
        };

        cpu = {
          interval = 5;
          format = "CPU {usage:2}%";
        };

        battery = {
          states = {
            good = 95;
            warning = 30;
            critical = 15;
          };
          format = "{icon} {capacity}%";
          "format-icons" = [
            ""
            ""
            ""
            ""
            ""
          ];
        };

        disk = {
          interval = 5;
          format = "Disk {percentage_used:2}%";
          path = "/";
        };

        tray = {
          "icon-size" = 20;
        };

        # Object Modules
        custom = {
          "up-right-arrow" = {
            format = "";
            tooltip = false;
          };
          "down-left-arrow" = {
            format = "";
            tooltip = false;
          };
          "up-left-arrow" = {
            format = "";
            tooltip = false;
          };
          "down-right-arrow" = {
            format = "";
            tooltip = false;
          };
        };
      }
    ];
  };
  home.file.".config/waybar/style.css".text = ''
    * {
    font-family: "Nerd Fonts Symbols Only", "Ariel", sans-serif;
    font-size: 12px;
    }

    window#waybar {
    background: #292b2e;
    color: #fdf6e3;
    }

    #custom-up-left-arrow,
    #custom-up-right-arrow,
    #custom-down-left-arrow,
    #custom-down-right-arrow {
    color: #1a1a1a;
    font-size: 27px;
    }

    #workspaces,
    #clock,
    #pulseaudio,
    #memory,
    #cpu,
    #battery,
    #disk,
    #tray {
    background: #1a1a1a;
    }

    #workspaces button {
    padding: 0 2px;
    color: #fdf6e3;
    }
    #workspaces button.focused {
    color: #268bd2;
    }
    #workspaces button:hover {
    box-shadow: inherit;
    text-shadow: inherit;
    }
    #workspaces button:hover {
    background: #1a1a1a;
    border: #1a1a1a;
    padding: 0 3px;
    }

    #pulseaudio {
    color: #268bd2;
    }
    #memory {
    color: #2aa198;
    }
    #cpu {
    color: #6c71c4;
    }
    #battery {
    color: #859900;
    }
    #disk {
    color: #b58900;
    }

    #clock,
    #pulseaudio,
    #memory,
    #cpu,
    #battery,
    #disk {
    padding: 0 10px;
    }
  '';
}
