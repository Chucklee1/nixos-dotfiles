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
            require('kanagawa').setup({
              colors = {
                palette = {
                  -- Bg Shades
                  sumiInk0 = "#16161D",
                  sumiInk1 = "#181820",
                  sumiInk2 = "#1a1a22",
                  sumiInk3 = "#1F1F28",
                  sumiInk4 = "#2A2A37",
                  sumiInk5 = "#363646",
                  sumiInk6 = "#54546D", --fg

                  -- Popup and Floats
                  waveBlue1 = "#223249",
                  waveBlue2 = "#2D4F67",

                  -- Diff and Git
                  winterGreen = "#2B3328",
                  winterYellow = "#49443C",
                  winterRed = "#43242B",
                  winterBlue = "#252535",
                  autumnGreen = "#76946A",
                  autumnRed = "#C34043",
                  autumnYellow = "#DCA561",

                  -- Diag
                  samuraiRed = "#E82424",
                  roninYellow = "#FF9E3B",
                  waveAqua1 = "#6A9589",
                  dragonBlue = "#658594",

                  -- Fg and Comments
                  oldWhite = "#C8C093",
                  fujiWhite = "#DCD7BA",
                  fujiGray = "#727169",

                  oniViolet = "#957FB8",
                  oniViolet2 = "#b8b4d0",
                  crystalBlue = "#7E9CD8",
                  springViolet1 = "#938AA9",
                  springViolet2 = "#9CABCA",
                  springBlue = "#7FB4CA",
                  lightBlue = "#A3D4D5", -- unused yet
                  waveAqua2 = "#7AA89F", -- improve lightness: desaturated greenish Aqua

                  -- waveAqua2  = "#68AD99",
                  -- waveAqua4  = "#7AA880",
                  -- waveAqua5  = "#6CAF95",
                  -- waveAqua3  = "#68AD99",

                  springGreen = "#98BB6C",
                  boatYellow1 = "#938056",
                  boatYellow2 = "#C0A36E",
                  carpYellow = "#E6C384",

                  sakuraPink = "#D27E99",
                  waveRed = "#E46876",
                  peachRed = "#FF5D62",
                  surimiOrange = "#FFA066",
                  katanaGray = "#717C7C",

                  dragonBlack0 = "#0d0c0c",
                  dragonBlack1 = "#12120f",
                  dragonBlack2 = "#1D1C19",
                  dragonBlack3 = "#181616",
                  dragonBlack4 = "#282727",
                  dragonBlack5 = "#393836",
                  dragonBlack6 = "#625e5a",

                  dragonWhite = "#c5c9c5",
                  dragonGreen = "#87a987",
                  dragonGreen2 = "#8a9a7b",
                  dragonPink = "#a292a3",
                  dragonOrange = "#b6927b",
                  dragonOrange2 = "#b98d7b",
                  dragonGray = "#a6a69c",
                  dragonGray2 = "#9e9b93",
                  dragonGray3 = "#7a8382",
                  dragonBlue2 = "#8ba4b0",
                  dragonViolet = "#8992a7",
                  dragonRed = "#c4746e",
                  dragonAqua = "#8ea4a2",
                  dragonAsh = "#737c73",
                  dragonTeal = "#949fb5",
                  dragonYellow = "#c4b28a", --"#a99c8b",
                  -- "#8a9aa3",

                  lotusInk1 = "#545464",
                  lotusInk2 = "#43436c",
                  lotusGray = "#dcd7ba",
                  lotusGray2 = "#716e61",
                  lotusGray3 = "#8a8980",
                  lotusWhite0 = "#d5cea3",
                  lotusWhite1 = "#dcd5ac",
                  lotusWhite2 = "#e5ddb0",
                  lotusWhite3 = "#f2ecbc",
                  lotusWhite4 = "#e7dba0",
                  lotusWhite5 = "#e4d794",
                  lotusViolet1 = "#a09cac",
                  lotusViolet2 = "#766b90",
                  lotusViolet3 = "#c9cbd1",
                  lotusViolet4 = "#624c83",
                  lotusBlue1 = "#c7d7e0",
                  lotusBlue2 = "#b5cbd2",
                  lotusBlue3 = "#9fb5c9",
                  lotusBlue4 = "#4d699b",
                  lotusBlue5 = "#5d57a3",
                  lotusGreen = "#6f894e",
                  lotusGreen2 = "#6e915f",
                  lotusGreen3 = "#b7d0ae",
                  lotusPink = "#b35b79",
                  lotusOrange = "#cc6d00",
                  lotusOrange2 = "#e98a00",
                  lotusYellow = "#77713f",
                  lotusYellow2 = "#836f4a",
                  lotusYellow3 = "#de9800",
                  lotusYellow4 = "#f9d791",
                  lotusRed = "#c84053",
                  lotusRed2 = "#d7474b",
                  lotusRed3 = "#e82424",
                  lotusRed4 = "#d9a594",
                  lotusAqua = "#597b75",
                  lotusAqua2 = "#5e857a",
                  lotusTeal1 = "#4e8ca2",
                  lotusTeal2 = "#6693bf",
                  lotusTeal3 = "#5a7785",
                  lotusCyan = "#d7e3d8",
                },
                theme = {
                  dragon = {
                    ui = {
                      fg         = palette.dragonWhite,
                      fg_dim     = palette.oldWhite,
                      fg_reverse = palette.waveBlue1,

                      bg_dim     = palette.dragonBlack1,
                      bg_gutter  = palette.dragonBlack4,

                      bg_m3      = palette.dragonBlack0,
                      bg_m2      = palette.dragonBlack1,
                      bg_m1      = palette.dragonBlack2,
                      bg         = palette.dragonBlack3,
                      bg_p1      = palette.dragonBlack4,
                      bg_p2      = palette.dragonBlack5,

                      special    = palette.dragonGray3,
                      whitespace = palette.dragonBlack6,
                      nontext    = palette.dragonBlack6,

                      bg_visual  = palette.waveBlue1,
                      bg_search  = palette.waveBlue2,

                      pmenu      = {
                        fg       = palette.fujiWhite,
                        fg_sel   = "none",
                        bg       = palette.waveBlue1,
                        bg_sel   = palette.waveBlue2,
                        bg_thumb = palette.waveBlue2,
                        bg_sbar  = palette.waveBlue1,
                      },

                      float      = {
                        fg        = palette.oldWhite,
                        bg        = palette.dragonBlack0,
                        fg_border = palette.sumiInk6,
                        bg_border = palette.dragonBlack0,
                      },
                    },
                    syn = {
                      string     = palette.dragonGreen2,
                      variable   = "none",
                      number     = palette.dragonPink,
                      constant   = palette.dragonOrange,
                      identifier = palette.dragonYellow,
                      parameter  = palette.dragonGray,
                      fun        = palette.dragonBlue2,
                      statement  = palette.dragonViolet,
                      keyword    = palette.dragonViolet,
                      operator   = palette.dragonRed,
                      preproc    = palette.dragonRed,
                      type       = palette.dragonAqua,
                      regex      = palette.dragonRed,
                      deprecated = palette.katanaGray,
                      punct      = palette.dragonGray2,
                      comment    = palette.dragonAsh,
                      special1   = palette.dragonTeal,
                      special2   = palette.dragonRed,
                      special3   = palette.dragonRed,
                    },
                    diag = {
                      error   = palette.samuraiRed,
                      ok      = palette.springGreen,
                      warning = palette.roninYellow,
                      info    = palette.dragonBlue,
                      hint    = palette.waveAqua1,
                    },
                    diff = {
                      add    = palette.winterGreen,
                      delete = palette.winterRed,
                      change = palette.winterBlue,
                      text   = palette.winterYellow,
                    },
                    vcs = {
                      added   = palette.autumnGreen,
                      removed = palette.autumnRed,
                      changed = palette.autumnYellow,
                    },
                    term = {
                      palette.dragonBlack0,  -- black
                      palette.dragonRed,     -- red
                      palette.dragonGreen2,  -- green
                      palette.dragonYellow,  -- yellow
                      palette.dragonBlue2,   -- blue
                      palette.dragonPink,    -- magenta
                      palette.dragonAqua,    -- cyan
                      palette.oldWhite,      -- white
                      palette.dragonGray,    -- bright black
                      palette.waveRed,       -- bright red
                      palette.dragonGreen,   -- bright green
                      palette.carpYellow,    -- bright yellow
                      palette.springBlue,    -- bright blue
                      palette.springViolet1, -- bright magenta
                      palette.waveAqua2,     -- bright cyan
                      palette.dragonWhite,   -- bright white
                      palette.dragonOrange,  -- extended color 1
                      palette.dragonOrange2, -- extended color 2
                    }
                  }
                }

              }
            })
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
