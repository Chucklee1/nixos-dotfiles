{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.services.nixvim.formatting;
in {
  options.services.nixvim.formatting = {
    enable = mkEnableOption {
      description = "enable formatting";
      default = true;
    };
    autoFormat = mkEnableOption {
      description = "enable automatic formatting";
      default = false;
    };
  };
  config = mkIf cfg.enable {
    plugins = {
      lsp-format.enable = true;
      none-ls = {
        enable = true;
        enableLspFormat = cfg.autoFormat;
        sources = {
          diagnostics.statix = {
            enable = true;
            settings.extra_args = ["--disable=duplicate_key"];
          };
          formatting = {
            alejandra.enable = true; # nix
            prettier.enable = true; # soyjack
            prettier.settings.disabled_filetypes = ["html"]; # tidy will cover html
            shfmt.enable = true;
            tidy.enable = true; # html, xhtml, xml
          };
        };
      };
    };
  };
}
