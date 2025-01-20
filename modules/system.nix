{
  lib,
  inputs,
  system,
  def,
  ...
}: {
  # -----------------------------------------------------------
  # boot
  # -----------------------------------------------------------
  boot = {
    initrd.systemd.enable = true; # force systemd to load early
    # enable "Silent Boot"
    consoleLogLevel = 0;
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
    # Hide the OS choice for bootloaders, press any key to bring up boot menu
    loader.timeout = 0;
    loader = {
      efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        efiSupport = true;
        device = "nodev";
      };
    };
  };

  # -----------------------------------------------------------
  # system options
  # -----------------------------------------------------------

  system.stateVersion = "24.05";
  networking = {
    networkmanager.enable = true;
    hostName = "${def.host}-${def.username}";
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

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {inherit system inputs def;};
    users.${def.username}.home = {
    stateVersion = "24.05"; # DO NOT CHANGE
    username = "${def.username}";
    homeDirectory = "/home/${def.username}";
  };};

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
  hardware.uinput.enable = true;
  programs.weylus.enable = true;
  services.udev.extraRules = ''
    KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
  '';

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [1701 9001];
    allowedUDPPortRanges = [
      {
        from = 4000;
        to = 4007;
      }
      {
        from = 8000;
        to = 8010;
      }
    ];
  };

  # misc
  services = {
    displayManager.ly.enable = true;
    printing.enable = true;
    fstrim.enable = true;
    tumbler.enable = true;
    gvfs.enable = true;
  };
}
