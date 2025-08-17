{
  inputs,
  self,
  ...
}: {
  global.home = [
    {
      programs = {
        git = {
          enable = true;
          userEmail = "kermitthefrog@kakao.com";
          userName = "Chucklee1";
        };
        lazygit = {
          enable = true;
          settings.notARepository = "skip";
          settings.promptToReturnFromSubprocess = false;
        };
        kitty = {
          enable = true;
          settings = {
            cursor_shape = "beam";
            background_blur = 40;
            confirm_os_window_close = 0;
            tab_bar_edge = "bottom";
            tab_bar_style = "powerline";
            tab_powerline_style = "round";
          };
        };
        btop.enable = true;
        direnv.enable = true;
        fzf.enable = true;
        zoxide = {
          enable = true;
          options = ["--cmd cd"];
        };
      };
    }
  ];

  additions = let
    base = {nixpkgs.overlays = [inputs.nix-vim.overlays.default];};
  in {
    core.nix = [base ({pkgs, ...}: {environment.systemPackages = [pkgs.nixvim.core];})];
    full.nix = [
      base
      ({
        pkgs,
        machine,
        ...
      }: {
        environment.systemPackages =
          if machine == "macbook"
          then [pkgs.nixvim.darwin]
          else [pkgs.nixvim.full];
      })
    ];
    full.home = [
      ({pkgs, ...}: {
        home.packages = [pkgs.rmpc];
        home.file.".config/rmpc".source = "${self}/assets/rmpc";
      })
    ];
  };

  macbook.nix = [
    ({config, ...}:
      with config.lib.stylix.colors; {
        services.sketchybar.enable = true;
        services.jankyborders = {
          enable = true;
          active_color = ''0xFF${base0D}'';
          inactive_color = ''0x00${base0D}'';
          style = "round";
          width = 3.0;
        };
      })
  ];
  macbook.home = [{home.file.".config/sketchybar".source = "${self}/assets/sketchybar";}];
}
