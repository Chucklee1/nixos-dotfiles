{self, ...}: {
  home-manager.sharedModules = [
    {
      programs.nixvim = {
        keymaps = [
  {
    key = "<leader>e";
    action = "<cmd>NvimTreeToggle<CR>";
    options = {
      silent = true;
      noremap = true;
    };
  }
  {
    key = "<leader>f";
    action = "<cmd>NvimTreeFocus<CR>";
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
