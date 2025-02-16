{inputs, ...}: {
  nix.global = [
    inputs.stylix.nixosModules.stylix
    ({pkgs, ...}: {
      # plymouth
      boot = {
        plymouth.enable = true;
        consoleLogLevel = 0;
        loader.timeout = 0;
        initrd.verbose = false;
        kernelParams = [
          "quiet"
          "splash"
          "boot.shell_on_fail"
          "loglevel=3"
          "rd.systemd.show_status=false"
          "rd.udev.log_level=3"
          "udev.log_priority=3"
        ];
      };
      stylix = {
        enable = true;
        autoEnable = true;
        homeManagerIntegration.autoImport = true;
        image = ../assets/wallpaper.png;
        base16Scheme = "${pkgs.base16-schemes}/share/themes/classic-dark.yaml";

        cursor.package = pkgs.bibata-cursors;
        cursor.name = "Bibata-Modern-Classic";
        cursor.size = 24;

        fonts = {
          monospace.package = pkgs.nerd-fonts.jetbrains-mono;
          monospace.name = "JetBrainsMono Nerd Font Mono";
          sansSerif.package = pkgs.noto-fonts-cjk-sans;
          sansSerif.name = "Noto Sans CJK";
          serif.package = pkgs.noto-fonts-cjk-serif;
          serif.name = "Noto Serif CJK";

          sizes = {
            applications = 12;
            terminal = 12;
            desktop = 11;
            popups = 12;
          };
        };
      };
    })
  ];

  home.global = [
    ({pkgs, ...}: {
      stylix = {
        iconTheme = {
          enable = true;
          package = pkgs.papirus-icon-theme;
          dark = "Papirus-Dark";
        };
      };
      gtk.enable = true;
      qt.enable = true;
    })
  ];
}
