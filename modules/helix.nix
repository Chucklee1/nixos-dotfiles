{
  programs.helix = {
    enable = true;
    settings = {
      editor = {
        lsp.display-messages = true;
        bufferline = "multiple";
        cursor-shape = {
          insert = "bar";
          normal = "block";
          select = "underline";
        };
      };
      keys.normal = {
        space.w = ":w";
        space.q = ":q";
        esc = ["collapse_selection" "keep_primary_selection"];
      };
    };
  };
}
