{
  nixvim,
  machine,
  ...
}: {
  nix.global = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        # dependancies
        libnotify
        libsecret
        # cli
        ripgrep
        pciutils
        wget
        git
        curl
      ];

      # programs
      programs.dconf.enable = true;
    })
  ];
  home.global = [
    ({
      lib,
      pkgs,
      ...
    }: {
      home.packages = with pkgs; [
        # dev tools
        openai
        rclone
        python3
        # apps
        qbittorrent
        muse-sounds-manager
        (ungoogled-chromium.override {enableWideVine = true;})
        logisim-evolution
        musescore
        nixvim
      ];

      programs = {
        # diagnostics
        btop.enable = true;
        #git
        git = {
          enable = true;
          userEmail = "kermitthefrog@kakao.com";
          userName = "Chucklee1";
        };
        # shell
        kitty = {
          enable = true;
          settings = {
            confirm_os_window_close = 0;
            tab_bar_edge = "bottom";
            tab_bar_style = lib.mkForce "powerline";
            tab_powerline_style = "round";
          };
        };

        bash = {
          enable = true;
          shellAliases =
            (lib.genAttrs ["v" "vi" "vm" "vim" "neovim"] (_: "nvim"))
            // {
              # nix - system installation
              disko-generate = ''sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode disko /tmp/disko.nix'';
              show-hardware = "sudo nixos-generate-config --show-hardware-config";
              install-mnt = "sudo nixos-install --root /mnt github:Chucklee1/nixos-dotfiles";
              # nix - general
              cg = "nix-collect-garbage";
              update-flake = "nix flake update --flake $HOME/nixos-dotfiles";
              rebuild-flake = "sudo nixos-rebuild switch -v --impure --show-trace --flake $HOME/nixos-dotfiles#${machine}";
              # tools - git
              clone-flake = ''
                if [ -e /home/goat/.ssh/id_ed25519.pub ]; then
                  echo "found ssh key, cloning with ssh..."
                  git clone git@github.com:Chucklee1/nixos-dotfiles
                else
                  echo "no ssh key found, trying tls..."
                  git clone https://github.com/Chucklee1/nixos-dotfiles
                fi
              '';
            };
        };
        oh-my-posh = {
          enable = true;
          enableBashIntegration = true;
          useTheme = "pure";
        };
      };
    })
  ];
}
