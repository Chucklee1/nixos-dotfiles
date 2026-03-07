{self, ...}: {
  # config based off of nixpkgs commit
  # 2ddc335e6f32b875e14ad9610101325b306a0add
  nix = [
    ({
      config,
      lib,
      pkgs,
      ...
    }:
      with lib; let
        cfg = config.services.xserver.windowManager.exwmFixed;
        loadScript = pkgs.writeText "emacs-exwm-load" ''
          ${cfg.loadScript}
        '';
        exwm-emacs = cfg.package;
      in {
        options = {
          services.xserver.windowManager.exwmFixed = {
            enable = mkEnableOption "exwm";
            loadScript = mkOption {
              default = "(require 'exwm)";
              description = ''
                Emacs lisp code to be run after loading the user's init
                file. If enableDefaultConfig is true, this will be run
                before loading the default config.
              '';
            };
            package = mkOption {
              default = pkgs.emacsWithPackages (epkgs: [epkgs.exwm]);
              description = ''
                Which emacs executable to use, including packages.
              '';
            };
          };
        };

        config = mkIf cfg.enable {
          services.xserver.windowManager.session = singleton {
            name = "exwm";
            start = ''${exwm-emacs}/bin/emacs -l ${loadScript}'';
          };
          environment.systemPackages = [exwm-emacs];
        };
      })
    ({pkgs, ...}: {
      services.xserver.enable = true;
      services.xserver.windowManager.exwmFixed = {
        enable = true;
        executable = pkgs.emacs;
        loadScript = "${builtins.readFile "${self}/pkgs/emacs/exwm.el"}";
      };
    })
  ];
}
