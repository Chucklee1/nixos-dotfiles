{self, ...}: {
  home.desktop = [
    ({config, ...}: {
      programs.wlogout.layout = [
        {
          label = "lock";
          action = "swaylock";
          text = "Lock";
          keybind = "l";
        }
        {
          label = "logout";
          action = "loginctl terminate-user $USER";
          text = "Logout";
          keybind = "e";
        }
        {
          label = "reboot";
          action = "systemctl reboot";
          text = "Reboot";
          keybind = "r";
        }
        {
          label = "shutdown";
          action = "systemctl poweroff";
          text = "Shutdown";
          keybind = "s";
        }
      ];
      programs.wlogout.style = with config.lib.stylix.colors.withHashtag; let
        root = "${self}/assets/wlogout";
      in ''
        * {
          orientation = horizontal;
        	background-image: none;
        	box-shadow: none;
        }

        window {
        	background-color: rgba(12, 12, 12, 0.9);
        }

        button {
            border-radius: 0;
            border-color: ${base00};
        	text-decoration-color: ${base06};
            color: ${base06};
        	background-color: ${base01};
        	border-style: solid;
        	border-width: 1px;
        	background-repeat: no-repeat;
        	background-position: center;
        	background-size: 25%;
        }

        button:focus, button:active, button:hover {
        	background-color: ${base0C};
        	outline-style: none;
        }

        #lock {
            background-image: image(url("${root}/lock.png"), url("${root}/lock.png"));
        }

        #logout {
            background-image: image(url("${root}/logout.png"), url("${root}/logout.png"));
        }

        #suspend {
            background-image: image(url("${root}/suspend.png"), url("${root}/suspend.png"));
        }

        #hibernate {
            background-image: image(url("${root}/hibernate.png"), url("${root}/hibernate.png"));
        }

        #shutdown {
            background-image: image(url("${root}/shutdown.png"), url("${root}/shutdown.png"));
        }

        #reboot {
            background-image: image(url("${root}/reboot.png"), url("${root}/reboot.png"));
        }
      '';
    })
  ];
}
