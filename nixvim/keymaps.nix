{
  globals.mapleader = " ";
  globals.maplocalleader = " ";
  keymaps = let
    mkKeymaps = mode: type: list:
      map (sublist:
        {
          inherit mode;
          key = builtins.elemAt sublist 1;
        }
        // (
          if type == "wrap"
          then {action = "<cmd>${builtins.elemAt sublist 0}<CR>";}
          else if type == "raw"
          then {
            action.__raw = builtins.elemAt sublist 0;
            options.desc = builtins.elemAt sublist 2;
          }
          else {}
        ))
      list;
  in
    builtins.concatLists [
      (mkKeymaps "n" "wrap"
        [
          # misc ui
          ["Oil" "<leader>e"]
          ["LazyGit" "<leader>gg"]

          # clear search results
          ["noh" "<esc>"]

          # Telescope binding
          ["Telescope find_files" "<leader>ff"]
          ["Telescope live_grep" "<leader>fn"]
          ["Telescope git_commits" "<leader>fg"]
          ["Telescope buffers" "<leader>fb"]

          # Buffer
          ["bn" "<S-l>"]
          ["bp" "<S-h>"]
          ["bd" "<leader>c"]
          ["bd!" "<leader>C"]
          ["Neotree toggle" "<leader>b"]

          # force it
          ["w!" "<leader>W"]
          ["q!" "<leader>Q"]
          ["bd" "<leader>c"]
          ["bd!" "<leader>C"]
        ])
      # set esc to also exit terminal mode
      (mkKeymaps "t" "raw" [[''[[<C-\><C-n>]]'' "<esc>" "escape terminal mode"]])

      # toggles - credit to Khaneliman's khanelivim
      (mkKeymaps "n" "raw" [
        [
          # lua
          ''
            function ()
              vim.b.disable_diagnostics = not vim.b.disable_diagnostics
              if vim.b.disable_diagnostics then
                vim.diagnostic.disable(0)
              else
                vim.diagnostic.enable(0)
              end
              vim.notify(string.format("Buffer Diagnostics %s", bool2str(not vim.b.disable_diagnostics), "info"))
            end''
          "<leader>ud"
          "local diagnostics toggle"
        ]
        [
          # lua
          ''
            function ()
              vim.g.disable_diagnostics = not vim.g.disable_diagnostics
              if vim.g.disable_diagnostics then
                vim.diagnostic.disable()
              else
                vim.diagnostic.enable()
              end
              vim.notify(string.format("Global Diagnostics %s", bool2str(not vim.g.disable_diagnostics), "info"))
            end''
          "<leader>uD"
          "Global diagnostics toggle"
        ]

        [
          # lua
          ''
            function ()
              vim.cmd('FormatToggle!')
              vim.notify(string.format("Buffer Autoformatting %s", bool2str(not vim.b[0].disable_autoformat), "info"))
            end''
          "<leader>uf"
          "local autoformatting toggle"
        ]

        [
          # lua
          ''
            function ()
              vim.cmd('FormatToggle')
              vim.notify(string.format("Global Autoformatting %s", bool2str(not vim.g.disable_autoformat), "info"))
            end''
          "<leader>uF"
          "Global autoformatting toggle"
        ]

        [
          # lua
          ''
            function ()
              if vim.g.spell_enabled then vim.cmd('setlocal nospell') end
              if not vim.g.spell_enabled then vim.cmd('setlocal spell') end
              vim.g.spell_enabled = not vim.g.spell_enabled
              vim.notify(string.format("Spell %s", bool2str(vim.g.spell_enabled), "info"))
            end''
          "<leader>us"
          "Spell toggle"
        ]

        [
          # lua
          ''
            function ()
              vim.wo.wrap = not vim.wo.wrap
              vim.notify(string.format("Wrap %s", bool2str(vim.wo.wrap), "info"))
            end''
          "<leader>uw"
          "Word Wrap toggle"
        ]
      ])
    ];
}
