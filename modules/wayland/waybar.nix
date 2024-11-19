{pkgs, ...}: {
  # waybar
  stylix.targets.waybar.enable = false;
  programs.waybar.enable = true;
  home = {
    file.".config/waybar/config.jsonc".text = ''
            // -*- mode: json -*-

        {
        	"layer": "top",
        	"position": "top",
        	"height": 30,

        	"modules-left": [
        		"niri/workspaces",
      			"custom/down-right-arrow",
        	],
        	"modules-center": [
      			"custom/up-left-arrow",
        		"clock#1",
        		"clock#2",
        		"clock#3",
      			"custom/down-righ-tarrow"
        	],
        	"modules-right": [
        		"custom/up-left-arrow",
        		"pulseaudio",
        		"memory",
        		"cpu",
        		"battery",
        		"disk",
        		"tray"
        	],

        	"custom/up-right-arrow": {
        		"format": "",
        		"tooltip": false
        	},
          "custom/down-left-arrow": {
        		"format": "",
        		"tooltip": false
        	},
        	"custom/up-left-arrow": {
        		"format": "",
        		"tooltip": false
        	},
      "custom/down-right-arrow": {
        		"format": "",
        		"tooltip": false
        	},

        	"niri/workspaces": {
        		"format": "{icon}",
        		"format-icons": {
        			"focused": "󰻀",
        			"default": ""
        		}
        	},

        	"clock#1": {
        		"format": "{:%a}",
        		"tooltip": false
        	},
        	"clock#2": {
        		"format": "{:%H:%M}",
        		"tooltip": false
        	},
        	"clock#3": {
        		"format": "{:%m-%d}",
        		"tooltip": false
        	},

        	"pulseaudio": {
        		"format": "{icon} {volume:2}%",
        		"format-bluetooth": "{icon}  {volume}%",
        		"format-muted": "MUTE",
        		"format-icons": {
        			"headphones": "",
        			"default": [
        				"",
        				""
        			]
        		},
        		"scroll-step": 5,
        		"on-click": "pamixer -t",
        		"on-click-right": "pavucontrol"
        	},
        	"memory": {
        		"interval": 5,
        		"format": "Mem {}%"
        	},
        	"cpu": {
        		"interval": 5,
        		"format": "CPU {usage:2}%"
        	},
        	"battery": {
        		"states": {
        			"good": 95,
        			"warning": 30,
        			"critical": 15
        		},
        		"format": "{icon} {capacity}%",
        		"format-icons": [
        			"",
        			"",
        			"",
        			"",
        			""
        		]
        	},
        	"disk": {
        		"interval": 5,
        		"format": "Disk {percentage_used:2}%",
        		"path": "/"
        	},
        	"tray": {
        		"icon-size": 20
        	}
        }
    '';
    file.".config/waybar/style.css".text = ''      
       * {
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
      #clock.1,
      #clock.2,
      #clock.3,
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
      }'';
  };
}
