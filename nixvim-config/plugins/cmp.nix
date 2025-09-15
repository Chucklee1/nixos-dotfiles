{
  lib,
  config,
  inputs,
  ...
}:
with lib; let
  cfg = config.services.nixvim.cmp;
in {
  options.services.nixvim.cmp.enable = mkEnableOption {
    description = "enables cmp options";
    default = false;
  };
  config = mkIf cfg.enable {
    opts.completeopt = [
      "menuone"
      "noselect"
      "noinsert"
    ];
    plugins = {
      # breadcrumbs
      lspsaga = {
        enable = true;
        settings.lightbulb = {
          virtual_text = false;
          enable = false;
        };
      };

      # ai
      copilot-lua.enable = true;
      copilot-chat.enable = true;

      # completion
      cmp.enable = true;
      cmp.autoEnableSources = true;
      cmp.settings = {
        mapping = {
          __raw = ''
            cmp.mapping.preset.insert({
              ['<C-d>'] = cmp.mapping.scroll_docs(-4),
              ['<C-f>'] = cmp.mapping.scroll_docs(4),
              ['<C-c>'] = cmp.mapping.abort(),
              ['<C-CR>'] = cmp.mapping.confirm({ select = true }),
            })
          '';
        };
        window = {
          completion.__raw = ''cmp.config.window.bordered()'';
          documentation.__raw = ''cmp.config.window.bordered()'';
        };
        sources = [
          {name = "async-path";}
          {name = "buffer";}
          {name = "dictionary";}
          {name = "emoji";}
          {name = "luasnip";}
          {name = "latex-symbols";}
          {name = "nvim_lsp";}
          {name = "spell";}
          {name = "treesitter";}
          {name = "vimtex";}
        ];
      };

      # snippets
      luasnip.enable = true;
      nvim-snippets.enable = true;
      friendly-snippets.enable = true;
    };
    # dictionary lookip file
    extraConfigLua = ''
      require("cmp_dictionary").setup {
        dic = {
          ["*"] = "${inputs.en_us-dictionary}/words.txt"
        },
      }
    '';
  };
}
