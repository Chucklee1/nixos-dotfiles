{self, ...}: {
  home-manager.sharedModules = [
    {
      programs.nixvim = {
        globals.mapleader = " ";
	keymaps = [
  {
    mode = ["n"];
    key = "<leader>f";
    action = "<cmd>NvimTreeToggle<CR>";
    options = {
      silent = true;
      noremap = true;
    };
  }
    {
    mode = ["n"];
    key = "<leader>h";
    action = "<cmd>BufferLineCycleNext<CR>";
    options = {
      silent = true;
      noremap = true;
    };
  }
    {
    mode = ["n"];
    key = "<leader>l";
    action = "<cmd>BufferLineCycleNext<CR>";
    options = {
      silent = true;
      noremap = true;
    };
  }
];
      };
    }
  ];
}
