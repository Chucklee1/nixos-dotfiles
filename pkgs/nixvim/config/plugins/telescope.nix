{
  plugins.telescope = {
    enable = true;
    settings = {
      defaults.mappings = {
        i = {"<C-d>".__raw = "require('telescope.actions').delete_buffer";};
        n = {"<C-d>".__raw = "require('telescope.actions').delete_buffer";};
      };
    };
  };

  plugins.nerdy.enable = true;
  plugins.nerdy.enableTelescope = true;
  extraConfigLua = "require('telescope').load_extension('nerdy')";
}
