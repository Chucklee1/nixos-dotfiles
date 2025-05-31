{
  globals.mapleader = " ";
  globals.maplocalleader = " ";
  keymaps = let
    mkKeymaps = mode: type: list:
      map (sublist: {
        inherit mode;
        key = builtins.elemAt sublist 1;
        action = (
          if type == "wrap"
          then "<cmd>${builtins.elemAt sublist 0}<CR>"
          else if type == "raw"
          then {__raw = builtins.elemAt sublist 0;}
          else {}
        );
      })
      list;
  in
    builtins.concatLists [
      (mkKeymaps "n" "wrap"
        [
          # misc ui
          ["Oil" "<leader>e"]
          ["LazyGit" "<leadecope find_files>gcope git_commits"]

          # clear search results
          ["noh" "<esc>"]

          # Telescope binding
          ["Telescope live_grep" "<leader>fn"]
          ["Telescope find_files" "<leader>f"]
          ["Telescope git_commits" "<leader>fg"]
          ["Telescope oldfiles" "<leader>fo"]

          # Bufferline bindings
          ["BufferLineCycleNext" "<S-l>"]
          ["BufferLineCyclePrev" "<S-h>"]

          # force it
          ["w!" "<leader>W"]
          ["q!" "<leader>Q"]
          ["bd" "<leader>c"]
          ["bd!" "<leader>C"]
        ])

      (mkKeymaps "v" "wrap" [
        # beter indenting
        ["<gv" "<S-Tab>"]
        ["<gv" "<"]
        [">gv" "<Tab>"]
        [">gv" ">"]
      ])
      # toggles
      (mkKeymaps "n" "raw" [
        [
          ''                      
            function ()
              vim.b.disable_diagnostics = not vim.b.disable_diagnostics
              if vim.b.disable_diagnostics then
                vim.diagnostic.disable(0)
              else
                vim.diagnostic.enable(0)
              end
            end''
          "<leader>ud"
        ]
        [
          ''
            function ()
              vim.g.disable_diagnostics = not vim.g.disable_diagnostics
              if vim.g.disable_diagnostics then
                vim.diagnostic.disable()
              else
                vim.diagnostic.enable()
              end
            end''
          "<leader>uD"
        ]

        [
          ''
            function ()
              -- vim.g.disable_autoformat = not vim.g.disable_autoformat
              vim.cmd('FormatToggle!')
            end''
          "<leader>uf"
        ]

        [
          ''
            function ()
              -- vim.g.disable_autoformat = not vim.g.disable_autoformat
              vim.cmd('FormatToggle')
            end''
          "<leader>uF"
        ]

        [
          ''
            function ()
              if vim.g.spell_enabled then vim.cmd('setlocal nospell') end
              if not vim.g.spell_enabled then vim.cmd('setlocal spell') end
              vim.g.spell_enabled = not vim.g.spell_enabled
            end''
          "<leader>uS"
        ]

        [
          ''
            function ()
              vim.wo.wrap = not vim.wo.wrap
            end''
          "<leader>uw"
        ]
      ])
    ];
}
