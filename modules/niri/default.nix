{
  config,
  lib,
  pkgs,
  inputs,
  def,
  ...
}: {
  imports = [inputs.niri.nixosModules.niri];
  nixpkgs.overlays = [inputs.niri.overlays.niri];

  programs.niri = {
    enable = true;
    package = pkgs.niri-unstable;
  };
  environment.systemPackages = with pkgs; [
    egl-wayland
    qt5.qtwayland
    qt6.qtwayland
    brightnessctl
    wev
    xwayland
    xwayland-run
    wl-clipboard
  ];
  xdg.portal = {
    enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
      pkgs.xdg-desktop-portal-gnome
    ];
    config.common.default = "*";
  };

  home-manager.sharedModules = [
    ./config.nix
    {
      programs = {
        fuzzel.enable = true;
        wpaperd.enable = true;
        swaylock = {
          enable = true;
          package = pkgs.swaylock-effects;
        };
        waybar = let
          colorWithHash = lib.mapAttrs (name: color: "#${color}") config.lib.stylix.colors;
          span = color: icon: "<span color='${color}' >${icon}</span>";
        in
          with colorWithHash; {
            enable = true;
            systemd.enable = true;
            settings = [
              {
                position = "top";
                layer = "top";
                height = 20;

                modules-left = [
                  "niri/window"
                ];
                modules-center = [
                  "clock#date"
                  "custom/seperator"
                  "clock#time"
                ];
                modules-right = [
                  "idle_inhibitor"
                  "pulseaudio"
                  "network"
                  "backlight"
                  "battery"
                  "tray"
                ];

                "clock#date" = {
                  format = "{:%Y-%M-%d}";
                  tooltip = false;
                };
                "custom/seperator" = {
                  format = " | ";
                  tooltip = false;
                };
                "clock#time" = {
                  format = "{:%H:%M:%S}";
                  interval = 1;
                  tooltip = false;
                };

                idle_inhibitor = {
                  format = "{icon}";
                  format-icons = {
                    activated = span base0C "";
                    deactivated = "";
                    tooltip = false;
                  };
                };
                pulseaudio = {
                  format = "{volume}% {icon}";
                  format-bluetooth = "{volume}% {icon}";
                  format-icons = {"default" = ["" "" ""];};
                  format-muted = span base08 "M ";
                  format-source = "{volume}% ";
                  format-source-muted = span base08 "M ";
                  on-click = "pavucontrol";
                  tooltip = false;
                };
                network = {
                  format-disconnected = "disconnected ⚠";
                  format-ethernet = "{ipaddr}/{cidr}";
                  format-wifi = "{essid} ({signalStrength}%) ";
                  on-click = "nmtui";
                  tooltip-format = "{ifname} via {gwaddr}";
                };
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
              }
            ];
            style = ''
              * {
                font-family: "JetBrainsMono nerd font";
                font-size: 12px;
              }
              #window {
                padding-left: 2px;
              }

              #idle_inhibitor,
              #pulseaudio,
              #network,
              #backlight,
              #battery {
                padding: 0 10px 0 10px;
              }
            '';
          };
      };
    }
  ];
}
