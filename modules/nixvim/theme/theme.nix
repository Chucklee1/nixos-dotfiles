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
        };
        kanagawa = {
          enable = true;
          settings = {
            theme = "dragon";
            background.dark = "dragon";
          };

          luaConfig.pre = ''
            ${builtins.readFile ./colors.lua}
            ${builtins.readFile ./theme.lua}
          '';
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
