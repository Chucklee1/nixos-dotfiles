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
    net.syncthing
    net.tailscale

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
    system.users
    system.network
    system.pkgconfig
    system.sys-specs

    drivers.graphical

    shell.variables
    shell.fish

    theming.stylix
    theming.themes.nord
  ];

  extraConfig = [
    inputs.disko.nixosModules.default
    inputs.impermanence.nixosModules.impermanence
    (import ../assets/disko/emphereal.nix {device = "/dev/nvme0n1";})
    ({
      lib,
      user,
      ...
    }: {
      boot.initrd = {
        enable = true;
        supportedFilesystems = ["nfs" "btrfs"];

        postResumeCommands = lib.mkAfter ''
          mkdir -p /mnt
          mount -o subvolid=5 /dev/nvme0n1p2 /mnt

          btrfs subvolume list -o /mnt/nixos/root |
          cut -f9 -d' ' |
          while read subvolume; do
              echo "deleting $subvolume subvolume..."
              btrfs subvolume delete "/mnt/$subvolume"
          done &&
          echo "deleting nixos/root subvolume..." &&
          btrfs subvolume delete /mnt/nixos/root

          echo "restoring blank nix/root subvolume..."
          btrfs subvolume snapshot /mnt/nixos/root_blank /mnt/nixos/root

          umount /mnt
        '';
      };

      fileSystems."/persist".neededForBoot = true;

      boot.loader.efi.efiSysMountPoint = "/boot/efi";
      boot.loader.grub.useOSProber = true;

      boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "sd_mod"];
      boot.kernelModules = ["kvm-intel"];

      hardware.cpu.intel.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;

      users.mutableUsers = false;
      users.users.${user}.hashedPasswordFile = "/persist/passwords/goat";

      # just to make sure
      services.openssh = {
        settings.challengeResponseAuthentication = false;
        extraConfig = lib.mkForce ''
          AllowTcpForwarding yes
          X11Forwarding no
          AllowAgentForwarding no
          AllowStreamLocalForwarding no
          AuthenticationMethods publickey
        '';
      };
      environment.persistence."/persist" = {
        hideMounts = true;
        directories = [
          "/var/log"
          "/var/lib/bluetooth"
          "/var/lib/nixos"
          "/var/lib/systemd/coredump"
          "/var/lib/tailscale"
          "/etc/NetworkManager/system-connections"
        ];
        files = [
          "/etc/machine-id"
        ];
        users.${user} = {
          directories = [
            "Downloads"
            "Music"
            "Pictures"
            "Documents"
            "Videos"
            "Repos"
            {
              directory = ".ssh";
              mode = "0700";
            }
            {
              directory = ".local/share/keyrings";
              mode = "0700";
            }
            ".local/share/direnv"
            ".local/share/zoxide"
            ".local/state/syncthing"
            ".cache/zen"
            ".zen"
          ];
        };
      };
      home-manager.users.${user} = {
        programs.fish.loginShellInit = ''
          set -g dotfiles $HOME/Repos/nixos-dotfiles
          set -g emacs_dir $HOME/.emacs.d

          # symlink dotfiles to user root
          if not test -d $HOME/nixos-dotfiles
              echo "symlinking $dotfiles to $HOME"
              ln -s $dotfiles $HOME
          else
              echo "$HOME/nixos-dotfiles already exist"
          end

          if not test -d $emacs_dir; mkdir $emacs_dir; end
          if test -d $emacs_dir/snippets; rm -rf $emacs_dir/snippets; end
          ln -s $dotfiles/pkgs/emacs/snippets $emacs_dir/

          # same for emacs config
          if not test -f $emacs_dir/init.el
              echo "symlinking $dotfiles/pkgs/emacs/config.el to $emacs_dir/init.el"
              ln -s $dotfiles/pkgs/emacs/config.el $emacs_dir/init.el
          else
              echo "$emacs_dir/init.el already exist"
          end

          set -e dotrepo
          set -e emacs_dir
        '';
      };
    })
  ];
}
