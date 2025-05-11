{
  plugins.telescope = {
    enable = true;
    extensions = {
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
        ];
      };
    };
  };
  extraConfigLua = ''
    require("telescope").load_extension("lazygit")
  '';
}
