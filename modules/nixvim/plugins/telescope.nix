{
  home.global = [
    {
      programs.nixvim.telescope = {
        enable = true;
        extensions = {
          media-files = {
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
    }
  ];
}
