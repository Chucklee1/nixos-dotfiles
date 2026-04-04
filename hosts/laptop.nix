{
  inputs,
  mod,
  ...
}:
with mod; {
  system = "x86_64-linux";
  builder = inputs.nixpkgs.lib.nixosSystem;
  user = "goat";
  modules = with mod; [
    services.syncthing
    services.tailscale

    programs.zen-browser
    programs.emacs
    programs.git
    programs.kitty
    programs.niri
    programs.waybar
    programs.yazi

    software.dev
    software.qol
    software.texlive

    system.boot
    system.home
    system.impermanence
    system.network
    system.pkgconfig
    system.sys-specs
    system.sops
    system.users

    services.graphical

    shell.variables
    shell.fish

    theming.stylix
    theming.blockgame
    theming.themes.nord
  ];

  extraConfig = [
    inputs.disko.nixosModules.default
    (import ../assets/disko/emphereal.nix {device = "/dev/nvme0n1";})
    {
      boot.initrd.supportedFilesystems = ["nfs" "btrfs"];
      boot.loader.efi.efiSysMountPoint = "/boot/efi";
      boot.loader.grub.useOSProber = true;

      boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "sd_mod"];
      boot.kernelModules = ["kvm-intel"];

      hardware.cpu.intel.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;
    }
    # impermenance setup
    {
      # persistance stuff
      services.impermanence = {
        device = "/dev/nvme0n1p2";
        root = {
          target = "nixos/root";
          blank = "nixos/root_blank";
        };
        persist = {
          system.directories = ["/var/lib/tailscale"];
          user.directories = [
            ".local/share/zoxide"
            ".local/share/direnv"
            ".local/state/syncthing"
            ".cache/zen"
            ".config/zen"
          ];
        };
      };
    }
    # sops - setup when login to machine
    ({config, user, ...}: {
      sops.age.keyFile = "/persist/secrets/age.keys.txt";
      users.users.${user}.hashedPasswordFile = config.sops.secrets."gregtrain/goat".path;
    })
    # symlink setup on login
    ({user, ...}: {
      home-manager.users.${user} = {
        programs.fish.loginShellInit = ''
          sln $HOME/Repos/nixos-dotfiles $HOME/
          sln $HOME/Repos/nixos-dotfiles/pkgs/emacs/config.el $HOME/.emacs.d/init.el
          sln $HOME/Repos/nixos-dotfiles/pkgs/emacs/snippets $HOME/.emacs.d/
        '';
      };
    })
  ];
}
