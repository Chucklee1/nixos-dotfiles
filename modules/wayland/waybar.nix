{pkgs, ...}: {
  # waybar
  home.packages = [(pkgs.nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})];
  stylix.targets.waybar.enable = false;
  programs.waybar.enable = true;
  home.file.".config/waybar/config.jsonc".text = ''
{
	"layer": "top",
	"position": "top",

	"modules-left": [
		"niri/workspaces",
    "idle_inhibitor",
		"custom/down-right-arrow"
	],
	"modules-center": [
		"custom/up-left-arrow",
		"clock#1",
		"clock#2",
		"clock#3",
    "custom/up-right-arrow"
	],
	"modules-right": [
    "custom/down-left-arrow",
		"pulseaudio",
		"memory",
		"cpu",
    "disk",
    "backlight",
		"battery",
		"custom/power",
	],

	"custom/up-left-arrow": {
		"format": "",
		"tooltip": false
	},
	"custom/up-right-arrow": {
		"format": "",
		"tooltip": false
	},
	"custom/down-left-arrow": {
		"format": "",
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
      "default": ""
    }
	},
  "idle_inhibitor": {
    "format": "{icon}",
    "format-icons": {
      "activated": "",
      "deactivated": ""
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
		"format": "{volume:2}% {icon} ",
		"format-bluetooth": "{volume}% {icon}",
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
		"format": "Mem {}% "
	},
	"cpu": {
		"interval": 5,
		"format": "{usage:2}% "
	},
	"disk": {
		"interval": 5,
		"format": "{percentage_used:2}% ",
		"path": "/"
	},
  "backlight": {
    "device": "intel_backlight",
    "format": "{percent}% {icon}",
    "format-icons": ["", "", "", "", "", "", "", "", ""]
  },
	"battery": {
		"states": {
			"good": 95,
			"warning": 30,
			"critical": 15
		},
		"format": "{capacity}% {icon}",
		"format-icons": [
			"",
			"",
			"",
			"",
			""
		]
	},
  "custom/power": {
    "format": "⏻",
    "on-click": "wlogout"
  }
}
  '';
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
    #idle_inhibitor,
    #clock,
    #pulseaudio,
    #memory,
    #cpu,
    #disk,
    #backlight,
    #battery,
    #custom-power {
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

    #custom-power,
    #pulseaudio {
    color: #268bd2;
    }
    #memory {
    color: #2aa198;
    }
    #cpu {
    color: #6c71c4;
    }
    #disk {
    color: #b58900;
    }
    #battery {
    color: #859900;
    }

    #idle_inhibitor,
    #clock,
    #pulseaudio,
    #memory,
    #cpu,
    #disk,
    #battery,
    #backlight,
    #custom-power {
    padding: 0 10px;
    }
  '';
}
