{
  config,
  lib,
  pkgs,
  def,
  ...
}:
def.module "default" {
  # -----------------------------------------------------------
  # boot
  # -----------------------------------------------------------
  boot.loader = {
    efi.canTouchEfiVariables = true;
    grub = {
      enable = true;
      efiSupport = true;
      device = "nodev";
      useOSProber = true;
    };
    grub2-theme = {
      enable = true;
      theme = "stylish";
      footer = true;
    };
  };

  # -----------------------------------------------------------
  # system options
  # -----------------------------------------------------------

  system.stateVersion = "24.05";
  networking = {
    networkmanager.enable = true;
    hostName = def.hostname;
  };
  i18n.defaultLocale = "en_CA.UTF-8";
  time.timeZone = "America/Vancouver";
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
  users.users.${def.username} = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "networkmanager"
      "uinput"
      "libvirtd"
      "audio"
      "video"
    ];
  };

  home-manager.users.${def.username}.home = {
    stateVersion = "24.05"; # DO NOT CHANGE
    username = "${def.username}";
    homeDirectory = "/home/${def.username}";
  };

  # -----------------------------------------------------------
  # security & polkit
  # -----------------------------------------------------------
  security = {
    polkit.enable = true;
    rtkit.enable = true; # for sound
  };
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "prohibit-password";
    };
  };

  # -----------------------------------------------------------
  # global drivers
  # -----------------------------------------------------------

  # audio
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # bluetooth
  hardware = {
    bluetooth.enable = true;
    bluetooth.powerOnBoot = true;
  };
  services.blueman.enable = true;

  # tablet support
  programs.weylus.enable = true;
  services.udev.extraRules = ''
    KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
  '';

  # misc
  services = {
    displayManager.ly.enable = true;
    printing.enable = true;
    fstrim.enable = true;
    tumbler.enable = true;
    gvfs.enable = true;
  };
}
def.module "desktop" {
  # boot
  boot.supportedFilesystems = ["ntfs"];

  # nvidia
  nixpkgs.config.nvidia.acceptLicense = true;
  services.xserver.videoDrivers = ["nvidia"];
  hardware = {
    graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = with pkgs; [
        vulkan-tools
        vulkan-loader
        libvdpau-va-gl
        ffmpeg
      ];
    };
    nvidia = {
      modesetting.enable = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
      forceFullCompositionPipeline = true;
      videoAcceleration = true;
      nvidiaSettings = true;
      open = false;
    };
  };
  environment.variables = {
    LIBVA_DRIVER_NAME = "nvidia";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    __GL_GSYNC_ALLOWED = "1";
    __GL_VRR_ALLOWED = "1";
    __GL_MaxFramesAllowed = "1";
  };
}
def.module "laptop" {
  services.xserver.videoDrivers = ["amdgpu"];
  hardware.amdgpu.amdvlk.enable = true;
  hardware.graphics = {
    enable32Bit = true;
    extraPackages = with pkgs; [
      vulkan-tools
      vulkan-loader
      libvdpau-va-gl
      ffmpeg
    ];
  };
}
