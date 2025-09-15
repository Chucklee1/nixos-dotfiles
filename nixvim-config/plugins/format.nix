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
          diagnostics = {
            statix = {
              enable = true;
              package = null;
              settings.extra_args = ["--disable=duplicate_key"];
            };
            zsh = {
              enable = true;
              package = null;
            };
          };
          formatting = {
            alejandra = {
              enable = true; # nix
              package = null;
            };
            prettier = {
              enable = true; # soyjack
              package = null;
              settings.disabled_filetypes = ["html"]; # tidy will cover html
            };
            shfmt = {
              enable = true;
              package = null;
            };
            tidy = {
              enable = true; # html, xhtml, xml
              package = null;
            };
          };
        };
      };
    };
  };
}
