require('kanagawa').setup({
  colors = {
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
