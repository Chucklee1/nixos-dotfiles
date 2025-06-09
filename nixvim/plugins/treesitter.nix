{pkgs, ...}: {
  plugins = {
    treesitter = {
      enable = true;
      settings = {
        highlight = {
          enable = true;
          disable = [
            "latex"
            "markdown"
          ];
        };
        incremental_selection.enable = true;
      };
    };
  };

  extraPackages = with pkgs; [tree-sitter];
}
