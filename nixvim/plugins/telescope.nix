{
  plugins.telescope = {
    enable = true;
    settings = {
      defaults.mappings = {
        i = {"<C-d>".__raw = "require('telescope.actions').delete_buffer";};
        n = {"<C-d>".__raw = "require('telescope.actions').delete_buffer";};
      };
    };
    extensions = {
      manix.enable = true;
      fzf-native.enable = true;
      media-files = {
        enable = true;
        settings.filetypes = [
          "png"
          "jpg"
          "webm"
          "gif"
          "mp4"
          "mov"
          "mkv"
          "pdf"
          "epub"
        ];
      };
    };
  };
  extraConfigLua = ''
    require("telescope").load_extension("lazygit")
  '';
}
