{
  home.global = [
    #({config, ...}:
    #with config.lib.stylix.colors.withHashtag;
    {
      stylix.targets.nixvim.enable = false;
      programs.nixvim.colorschemes = {
        nightfox = {
          enable = false;
          flavor = "nordfox";
          settings.palettes.nordfox = {
            black = "#3b4252";
            red = "#bf616a";
            green = "#a3be8c";
            yellow = "#ebcb8b";
            blue = "#81a1c1";
            magenta = "#b48ead";
            cyan = "#88c0d0";
            white = "#e5e9f0";
            orange = "#c9826b";
            pink = "#bf88bc";

            comment = "#60728a";

            bg0 = "#232831"; # Dark bg (status line and float)
            bg1 = "#2e3440"; # Default bg
            bg2 = "#39404f"; # Lighter bg (colorcolm folds)
            bg3 = "#444c5e"; # Lighter bg (cursor line)
            bg4 = "#5a657d"; # Conceal, border fg

            fg0 = "#c7cdd9"; # Lighter fg
            fg1 = "#cdcecf"; # Default fg
            fg2 = "#abb1bb"; # Darker fg (status line)
            fg3 = "#7e8188"; # Darker fg (line numbers; fold colums)

            sel0 = "#3e4a5b"; # Popup bg, visual selection bg
            sel1 = "#4f6074"; # Popup sel bg, search bg
          };
        };
        kanagawa = {
          enable = true;
          settings = {
            theme = "dragon";
            background.dark = "dragon";
          };
        };
      };
    }
  ];
  /*
  base00: "#151515"
  base01: "#202020"
  base02: "#303030"
  base03: "#505050"
  base04: "#B0B0B0"
  base05: "#D0D0D0"
  base06: "#E0E0E0"
  base07: "#F5F5F5"
  base08: "#AC4142"
  base09: "#D28445"
  base0A: "#F4BF75"
  base0B: "#90A959"
  base0C: "#75B5AA"
  base0D: "#6A9FB5"
  base0E: "#AA759F"
  base0F: "#8F5536"
  */
}
