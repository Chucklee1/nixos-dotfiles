{
  home.global = [
    ({pkgs, ...}: {
      stylix.targets.nixvim.enable = false;
      programs.nixvim.extraPlugins = [
        (pkgs.vimUtils.buildVimPlugin {
          name = "hybrid.nvim";
          src = pkgs.fetchFromGitHub {
            owner = "HoNamDuong";
            repo = "hybrid.nvim";
            rev = "5f4003e71ef9b1d110e07affde6711d570f2ef5a";
            hash = "sha256-v7kVgBODaUPZ4woygRSyIcDN+Pdsp3Xm/eVlK7zIkUo=";
          };
        })
      ];
      programs.nixvim.colorscheme = "hybrid";
      programs.nixvim.colorschemes = {
        gruvbox.enable = false;
        nightfox = {
          enable = false;
          flavor = "nordfox";
        };
        kanagawa = {
          enable = false;
          settings = {
            theme = "dragon";
            background.dark = "dragon";
            palette = {
              # Bg Shades
              sumiInk0 = "#16161D";
              sumiInk1 = "#181820";
              sumiInk2 = "#1a1a22";
              sumiInk3 = "#1F1F28";
              sumiInk4 = "#2A2A37";
              sumiInk5 = "#363646";
              sumiInk6 = "#54546D"; #fg

              # Popup and Floats
              waveBlue1 = "#223249";
              waveBlue2 = "#2D4F67";

              # Diff and Git
              winterGreen = "#2B3328";
              winterYellow = "#49443C";
              winterRed = "#43242B";
              winterBlue = "#252535";
              autumnGreen = "#76946A";
              autumnRed = "#C34043";
              autumnYellow = "#DCA561";

              # Diag
              samuraiRed = "#E82424";
              roninYellow = "#FF9E3B";
              waveAqua1 = "#6A9589";
              dragonBlue = "#658594";

              # Fg and Comments
              oldWhite = "#C8C093";
              fujiWhite = "#DCD7BA";
              fujiGray = "#727169";

              oniViolet = "#957FB8";
              oniViolet2 = "#b8b4d0";
              crystalBlue = "#7E9CD8";
              springViolet1 = "#938AA9";
              springViolet2 = "#9CABCA";
              springBlue = "#7FB4CA";
              lightBlue = "#A3D4D5"; # unused yet
              waveAqua2 = "#7AA89F"; # improve lightness: desaturated greenish Aqua

              springGreen = "#98BB6C";
              boatYellow1 = "#938056";
              boatYellow2 = "#C0A36E";
              carpYellow = "#E6C384";

              sakuraPink = "#D27E99";
              waveRed = "#E46876";
              peachRed = "#FF5D62";
              surimiOrange = "#FFA066";
              katanaGray = "#717C7C";

              dragonBlack0 = "#0d0c0c";
              dragonBlack1 = "#12120f";
              dragonBlack2 = "#1D1C19";
              dragonBlack3 = "#181616";
              dragonBlack4 = "#282727";
              dragonBlack5 = "#393836";
              dragonBlack6 = "#625e5a";

              dragonWhite = "#c5c9c5";
              dragonGreen = "#87a987";
              dragonGreen2 = "#8a9a7b";
              dragonPink = "#a292a3";
              dragonOrange = "#b6927b";
              dragonOrange2 = "#b98d7b";
              dragonGray = "#a6a69c";
              dragonGray2 = "#9e9b93";
              dragonGray3 = "#7a8382";
              dragonBlue2 = "#8ba4b0";
              dragonViolet = "#8992a7";
              dragonRed = "#c4746e";
              dragonAqua = "#8ea4a2";
              dragonAsh = "#737c73";
              dragonTeal = "#949fb5";
              dragonYellow = "#c4b28a";

              lotusInk1 = "#545464";
              lotusInk2 = "#43436c";
              lotusGray = "#dcd7ba";
              lotusGray2 = "#716e61";
              lotusGray3 = "#8a8980";
              lotusWhite0 = "#d5cea3";
              lotusWhite1 = "#dcd5ac";
              lotusWhite2 = "#e5ddb0";
              lotusWhite3 = "#f2ecbc";
              lotusWhite4 = "#e7dba0";
              lotusWhite5 = "#e4d794";
              lotusViolet1 = "#a09cac";
              lotusViolet2 = "#766b90";
              lotusViolet3 = "#c9cbd1";
              lotusViolet4 = "#624c83";
              lotusBlue1 = "#c7d7e0";
              lotusBlue2 = "#b5cbd2";
              lotusBlue3 = "#9fb5c9";
              lotusBlue4 = "#4d699b";
              lotusBlue5 = "#5d57a3";
              lotusGreen = "#6f894e";
              lotusGreen2 = "#6e915f";
              lotusGreen3 = "#b7d0ae";
              lotusPink = "#b35b79";
              lotusOrange = "#cc6d00";
              lotusOrange2 = "#e98a00";
              lotusYellow = "#77713f";
              lotusYellow2 = "#836f4a";
              lotusYellow3 = "#de9800";
              lotusYellow4 = "#f9d791";
              lotusRed = "#c84053";
              lotusRed2 = "#d7474b";
              lotusRed3 = "#e82424";
              lotusRed4 = "#d9a594";
              lotusAqua = "#597b75";
              lotusAqua2 = "#5e857a";
              lotusTeal1 = "#4e8ca2";
              lotusTeal2 = "#6693bf";
              lotusTeal3 = "#5a7785";
              lotusCyan = "#d7e3d8";
            };
            colors = {
              theme.dragon = {
                ui = {
                  fg = "dragonWhite";
                  fg_dim = "oldWhite";
                  fg_reverse = "waveBlue1";

                  bg_dim = "dragonBlack1";
                  bg_gutter = "dragonBlack4";

                  bg_m3 = "dragonBlack0";
                  bg_m2 = "dragonBlack1";
                  bg_m1 = "dragonBlack2";
                  bg = "dragonBlack2"; # 3
                  bg_p1 = "dragonBlack4";
                  bg_p2 = "dragonBlack5";

                  special = "dragonGray3";
                  whitespace = "dragonBlack6";
                  nontext = "dragonBlack6";

                  bg_visual = "waveBlue1";
                  bg_search = "waveBlue2";

                  pmenu = {
                    fg = "fujiWhite";
                    fg_sel = "none";
                    bg = "waveBlue1";
                    bg_sel = "waveBlue2";
                    bg_thumb = "waveBlue2";
                    bg_sbar = "waveBlue1";
                  };

                  float = {
                    fg = "oldWhite";
                    bg = "dragonBlack0";
                    fg_border = "sumiInk6";
                    bg_border = "dragonBlack0";
                  };
                };
                syn = {
                  string = "dragonGreen2";
                  variable = "none";
                  number = "dragonPink";
                  constant = "dragonOrange";
                  identifier = "dragonYellow";
                  parameter = "dragonGray";
                  fun = "dragonBlue2";
                  statement = "dragonViolet";
                  keyword = "dragonViolet";
                  operator = "dragonRed";
                  preproc = "dragonRed";
                  type = "dragonAqua";
                  regex = "dragonRed";
                  deprecated = "katanaGray";
                  punct = "dragonGray2";
                  comment = "dragonAsh";
                  special1 = "dragonTeal";
                  special2 = "dragonRed";
                  special3 = "dragonRed";
                };
                diag = {
                  error = "samuraiRed";
                  ok = "springGreen";
                  warning = "roninYellow";
                  info = "dragonBlue";
                  hint = "waveAqua1";
                };
                diff = {
                  add = "winterGreen";
                  delete = "winterRed";
                  change = "winterBlue";
                  text = "winterYellow";
                };
                vcs = {
                  added = "autumnGreen";
                  removed = "autumnRed";
                  changed = "autumnYellow";
                };
              };
            };
          };
        };
      };
    })
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
