{
  config,
  pkgs,
  ...
}: {
  programs.waybar.enable = true;

  programs.waybar.settings = {
    mainBar = {
      layer = "top";
      position = "top";
      height = 26;
      output = [
        "eDP-1"
      ];

      modules-left = ["custom/logo"];
      modules-right = ["clock" "battery"];

      "custom/logo" = {
        format = "";
        tooltip = false;
        on-click = ''fuzzel'';
      };

      "clock" = {
        interval = 60;
        format = "{:%a %d/%m %I:%M}";
      };

      "battery" = {
        tooltip = false;
      };
    };
  };

  programs.waybar.style = ''

    * {
      border: none;
      border-radius: 0;
      padding: 0;
      margin: 0;
      font-size: 11px;
    }

    window#waybar {
      background: #292828;
      color: #ffffff;
    }

    #custom-logo {
      font-size: 18px;
      margin: 0;
      margin-left: 7px;
      margin-right: 12px;
      padding: 0;
      font-family: NotoSans Nerd Font Mono;
    }

    #clock {
      margin-left: 5px;
      margin-right: 5px;
    }

    #battery {
      margin-left: 7px;
      margin-right: 3px;
    }
  '';
}
