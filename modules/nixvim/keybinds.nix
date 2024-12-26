_: {
  keymaps = [
    # lazygit
    {
      mode = "n";
      key = "<leader>gg";
      action = "<cmd>LazyGit<CR>";
      options = {
        desc = "LazyGit (root dir)";
      };
    }
    # toggleterm
    {
      mode = "n";
      action = "<cmd>ToggleTerm<CR>";
      key = "<leader>t";
      options = {
        noremap = true;
        silent = true;
      };
    }
    {
      mode = "t";
      action = "<cmd>ToggleTerm<CR>";
      key = "<leader>x";
      options = {
        noremap = true;
        silent = true;
      };
    }
    # bufferline
    {
      mode = "n";
      key = "<Tab>";
      action = "<cmd>BufferLineCycleNext<cr>";
      options = {
        desc = "Cycle to next buffer";
      };
    }

    {
      mode = "n";
      key = "<S-Tab>";
      action = "<cmd>BufferLineCyclePrev<cr>";
      options = {
        desc = "Cycle to previous buffer";
      };
    }

    {
      mode = "n";
      key = "<S-l>";
      action = "<cmd>BufferLineCycleNext<cr>";
      options = {
        desc = "Cycle to next buffer";
      };
    }

    {
      mode = "n";
      key = "<S-h>";
      action = "<cmd>BufferLineCyclePrev<cr>";
      options = {
        desc = "Cycle to previous buffer";
      };
    }
    {
      action = "<cmd>bd<CR>";
      key = "<leader>c";
      options = {
        noremap = true;
        silent = true;
      };
    }
  ];
}
