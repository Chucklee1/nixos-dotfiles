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
        wlogout.enable = true;
        wpaperd.enable = true;
        swaylock = {
          enable = true;
          package = pkgs.swaylock-effects;
        };
        waybar = with (def.files {inherit config lib;}); {
          enable = true;
          systemd.enable = true;
          settings = [
            {
              position = "top";
              layer = "top";
              height = 20;

              modules-left = ["niri/workspace" "niri/window"];

              modules-center = ["clock"];

              clock = {
                "format" = "{ :%Y-%m-%d | %H:%M:%S}";
                "interval" = 1;
              };

              modules-right = [
                "idle_inhibitor"
                "pulseaudio"
                "network"
                "backlight"
                "battery"
                "tray"
                "custom/power"
              ];

              idle_inhibitor = {
                "format" = "{icon}";
                "format-icons" = {
                  "activated" = "";
                  "deactivated" = "";
                };
              };
              pulseaudio = {
                "format" = "{volume}% {icon}";
                "format-bluetooth" = "{volume}% {icon}";
                "format-icons" = {"default" = ["" "" ""];};
                "format-muted" = "M ";
                "format-source" = "{volume}% ";
                "format-source-muted" = "";
                "on-click" = "pavucontrol";
              };
              network = {
                "format-alt" = "{ifname} = {ipaddr}/{cidr}";
                "format-disconnected" = "disconnected ⚠";
                "format-ethernet" = "{ipaddr}/{cidr} 󰊗";
                "format-wifi" = "{essid} ({signalStrength}%) ";
                "tooltip-format" = "{ifname} via {gwaddr} 󰊗";
              };
              backlight = {
                "format" = "{percent}% {icon}";
                "format-icons" = ["" "" "" "" "" "" "" "" ""];
              };
              battery = {
                "interval" = 1;
                "states" = {
                  "warning" = 30;
                  "critical" = 20;
                };
                "format-icons" = [" " " " " " " " " "];
                "format" = "{capacity}% {format-charging}{icon}";
                "format-alt" = "{icon} {time}";
                "tooltip" = false;
              };
              tray = {"spacing" = 10;};
              "custom/power" = {
                "format" = "⏻ ";
                "on-click" = "wlogout";
              };
            }
          ];
          style = waybarStyle;
        };
      };
    }
  ];
}
