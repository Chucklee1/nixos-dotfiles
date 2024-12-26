{
  lib,
  config,
  pkgs,
  def,
  ...
}: {
  imports = [./hardware.nix];
  # -----------------------------------------------------------
  # system
  # -----------------------------------------------------------
  boot = {
    initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
    kernelModules = ["kvm-amd"];
    supportedFilesystems = ["ntfs"];
  };

  system.stateVersion = "24.05"; # DO NOT CHANGE
  networking = {
    interfaces.enp7s0.useDHCP = lib.mkDefault true;
    interfaces.wlp6s0.useDHCP = lib.mkDefault true;
  };

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia = {
    powerManagement.enable = false;
    powerManagement.finegrained = false;
    open = false;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  # -----------------------------------------------------------
  # packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # tools/deps
    nvidia-vaapi-driver
    zenity
    wineWowPackages.stagingFull
    samba
    winetricks
    protonup-qt
    protontricks
    # hyprland
    hyprland-protocols
    hyprshot
    hypridle
    hyprlock
    hyprsunset
    # apps/games
    webcord
    osu-lazer-bin
    prismlauncher
  ];

  # -----------------------------------------------------------
  # programs
  # -----------------------------------------------------------
  programs = {
    hyprland.enable = true;
    virt-manager.enable = true;
    dconf.enable = true;
    gamemode.enable = true;
    steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
      localNetworkGameTransfers.openFirewall = true;
    };
  };

  # -----------------------------------------------------------
  # misc
  # -----------------------------------------------------------
  environment.variables = {STEAM_EXTRA_COMPAT_TOOLS_PATHS = "~/.steam/root/compatibilitytools.d";};
  virtualisation = {
    spiceUSBRedirection.enable = true;
    libvirtd = {
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        runAsRoot = true;
        swtpm.enable = true;
        ovmf = {
          enable = true;
          packages = [
            (pkgs.OVMF.override {
              secureBoot = true;
              tpmSupport = true;
            })
            .fd
          ];
        };
      };
    };
  };

  # -----------------------------------------------------------
  # home manager
  # -----------------------------------------------------------
  home-manager.sharedModules = [
    {
      dconf.settings = {
        "org/virt-manager/virt-manager/connections" = {
          autoconnect = ["qemu:///system"];
          uris = ["qemu:///system"];
        };
      };
      wayland.windowManager.hyprland = import ./hyprland.nix;
    }
  ];
}
