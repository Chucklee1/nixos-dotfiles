{
  home.global = [
    ({config, ...}:
      with config.lib.stylix.colors.withHashtag; {
        stylix.targets.nixvim.enable = false;
        programs.nixvim.colorschemes.nightfox = {
          enable = false;
          flavor = "nordfox";
          settings.palettes.nordfox = {
            black = "${base00}";
            red = "${base08}";
            green = "${base0B}";
            yellow = "${base0A}";
            blue = "${base0d}";
            magenta = "${base0E}";
            cyan = "${base0C}";
            white = "${base07}";
            orange = "${base09}";
            pink = "#bf88bc";

            comment = "${base02}";

            bg0 = "#0F0F0F"; # Dark bg (status line and float)
            bg1 = "${base00}"; # Default bg
            bg2 = "${base02}"; # Lighter bg (colorcolm folds)
            bg3 = "#404040"; # Lighter bg (cursor line)
            bg4 = "${base03}"; # Conceal, border fg

            fg0 = "${base05}"; # Lighter fg
            fg1 = "${base05}"; # Default fg
            fg2 = "#404040"; # Darker fg (status line)
            fg3 = "${base05}"; # Darker fg (line numbers; fold colums)

            sel0 = "${base01}"; # Popup bg, visual selection bg
            sel1 = "${base01}"; # Popup sel bg, search bg
          };
        };
      })
  ];
}
