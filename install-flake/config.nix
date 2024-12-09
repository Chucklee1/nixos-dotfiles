{
  config,
  lib,
  pkgs,
  modulesPath,
  inputs,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  # -----------------------------------------------------------
  # boot options
  # -----------------------------------------------------------
  boot = {
    initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
    initrd.kernelModules = [];
    kernelModules = ["kvm-amd"];
    extraModulePackages = [];
    loader = {
      efi.efiSysMountPoint = "/boot";
      efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        efiSupport = true;
        device = "nodev";
      };
    };
  };

  # -----------------------------------------------------------
  # partitioning
  # -----------------------------------------------------------
  disko.devices.disk.main = {
    # When using disko-install, we will overwrite this value from the commandline
    device = "/dev/disk/by-id/some-disk-id";
    type = "disk";
    content.type = "gpt";
    content.partitions = {
      MBR = {
        type = "EF02"; # for grub MBR
        size = "1M";
      };
      ESP = {
        type = "EF00";
        size = "1G";
        content = {
          type = "filesystem";
          format = "vfat";
          mountpoint = "/boot";
        };
      };
      root = {
        size = "100%";
        content = {
          type = "filesystem";
          format = "ext4";
          mountpoint = "/";
        };
      };
    };
  };

  # -----------------------------------------------------------
  # system options
  # -----------------------------------------------------------
  system.stateVersion = "24.05"; # DO NOT CHANGE
  networking = {
    hostName = "goat";
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
    interfaces.wlp6s0.useDHCP = lib.mkDefault true;
  };
  time.timeZone = "America/Vancouver";
  i18n.defaultLocale = "en_CA.UTF-8";
  console = {
    earlySetup = true;
    keyMap = "us";
  };

  # -----------------------------------------------------------
  # nix options
  # -----------------------------------------------------------
  nixpkgs = {
    hostPlatform = lib.mkDefault "x86_64-linux";
    config.allowUnfree = true;
  };
  nix.settings = {
    auto-optimise-store = true;
    experimental-features = ["nix-command" "flakes"];
  };

  # -----------------------------------------------------------
  # user
  # -----------------------------------------------------------
  users.users.goat = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
  };

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {inherit inputs;};
    users.goat = {
      home.stateVersion = "24.05"; # DO NOT CHANGE
      home.username = "goat";
      home.homeDirectory = "/home/goat";
    };
  };

  # -----------------------------------------------------------
  # packages and firmware
  # -----------------------------------------------------------
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  environment.systemPackages = with pkgs; [
    wget
    git
    curl
    neovim
  ];
}
