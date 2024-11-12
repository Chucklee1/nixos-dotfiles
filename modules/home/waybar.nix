{...}: {
  programs.waybar = {
    enable = true;
    settings = [
      {
        layer = "top";
        position = "top";
        mod = "dock";
        exclusive = true;
        gtk-layer-shell = true;
        margin-bottom = -1;
        passthrough = false;
        height = 35;

        modules-lexft = ["custom/os_button"];
        modules-center = [];
        modules-right = [
          "cpu"
          "temperature"
          "memory"
          "disk"
          "tray"
          "pulseaudio"
          "network"
          "battery"
          "clock"
        ];

        custom.os_button = {
          format = "";
          on-click = "fuzzel";
          tooltip = false;
        };

        cpu = {
          interval = 5;
          format = "  {usage}%";
          max-length = 10;
        };

        temperature = {
          hwmon-path-abs = "/sys/devices/platform/coretemp.0/hwmon";
          input-filename = "temp2_input";
          critical-threshold = 75;
          tooltip = false;
          format-critical = "({temperatureC}°C)";
          format = "({temperatureC}°C)";
        };

        disk = {
          interval = 30;
          format = "󰋊 {percentage_used}%";
          path = "/";
          tooltip = true;
          unit = "GB";
          tooltip-format = "Available {free} of {total}";
        };

        memory = {
          interval = 10;
          format = "  {percentage}%";
          max-length = 10;
          tooltip = true;
          tooltip-format = "RAM - {used:0.1f}GiB used";
        };

        tray = {
          icon-size = 18;
          spacing = 3;
        };

        clock = {
          format = "      {:%R}";
          tooltip-format = "<tt><small>{calendar}</small></tt>";
          calendar = {
            mode = "year";
            mode-mon-col = 3;
            weeks-pos = "right";
            on-scroll = 1;
            on-click-right = "mode";
            format = {
              months = "<span color='#ffead3'><b>{}</b></span>";
              days = "<span color='#ecc6d9'><b>{}</b></span>";
              weeks = "<span color='#99ffdd'><b>W{}</b></span>";
              weekdays = "<span color='#ffcc66'><b>{}</b></span>";
              today = "<span color='#ff6699'><b><u>{}</u></b></span>";
            };
          };
          actions = {
            on-click-right = "mode";
            on-click-forward = "tz_up";
            on-click-backward = "tz_down";
            on-scroll-up = "shift_up";
            on-scroll-down = "shift_down";
          };
        };

        network = {
          format-wifi = "{icon}";
          format-ethernet = "  ";
          format-disconnected = "󰌙";
          format-icons = [
            "󰤯 "
            "󰤟 "
            "󰤢 "
            "󰤢 "
            "󰤨 "
          ];
        };

        battery = {
          states = {
            good = 95;
            warning = 30;
            critical = 20;
          };
          format = "{icon} {capacity}%";
          format-charging = " {capacity}%";
          format-plugged = " {capacity}%";
          format-alt = "{time} {icon}";
          format-icons = [
            "󰂎"
            "󰁺"
            "󰁻"
            "󰁼"
            "󰁽"
            "󰁾"
            "󰁿"
            "󰂀"
            "󰂁"
            "󰂂"
            "󰁹"
          ];
        };

        pulseaudio = {
          max-volume = 150;
          scroll-step = 10;
          format = "{icon}";
          tooltip-format = "{volume}%";
          format-muted = " ";
          format-icons = {
            default = [
              " "
              " "
              " "
            ];
          };
          on-click = "pwvucontrol";
        };
      }
    ];
  };
}
