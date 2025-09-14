{pkgs, ...}: {
  plugins = {
    treesitter = {
      enable = true;
      settings = {
        highlight.enable = true;
        incremental_selection.enable = true;
      };
    };
  };

  extraPackages = with pkgs; [tree-sitter];
}
