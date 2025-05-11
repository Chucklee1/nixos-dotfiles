{
  inputs,
  system,
  user,
  machine,
  ...
}: {
  nix.global = [
    # ---- system ----
    inputs.home-manager.nixosModules.home-manager
    ({
      lib,
      config,
      ...
    }: {
      # boot
      boot.loader = {
        efi.canTouchEfiVariables = true;
        grub = {
          enable = true;
          efiSupport = true;
          device = "nodev";
        };
      };

      # general
      system.stateVersion = "24.05";
      networking = {
        useDHCP = lib.mkDefault true;
        hostName = "${user}-${machine}";
        networkmanager.enable = true;
      };
      i18n.defaultLocale = "en_CA.UTF-8";
      time.timeZone = "America/Vancouver";

      # nix
      nixpkgs = {
        inherit system;
        config.allowUnfree = true;
      };
      nix.settings = {
        auto-optimise-store = true;
        experimental-features = ["nix-command" "flakes"];
      };

      # user
      users.users.${user} = {
        name = "${user}";
        isNormalUser = true;
        extraGroups = [
          "wheel"
          "networkmanager"
          "audio"
          "video"
          "libvirtd"
        ];
      };

      home-manager.users.${user} = {
        home = {
          stateVersion = "24.05"; # DO NOT CHANGE
          username = "${config.users.users.${user}.name}";
          homeDirectory = "/home/${config.users.users.${user}.name}";
        };
        nixpkgs.config.allowUnfree = true;
        imports = config._module.args.homeMods;
      };
    })
    # ---- higher-level drivers ----
    ({pkgs, ...}: {
      # gpu
      hardware.graphics = {
        enable = true;
        enable32Bit = true;
        extraPackages = with pkgs; [
          vulkan-tools
          vulkan-loader
          libvdpau-va-gl
        ];
      };

      # audio
      security.rtkit.enable = true;
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

      # misc
      services = {
        printing.enable = true;
        fstrim.enable = true;
        tumbler.enable = true;
        gvfs.enable = true;
      };
    })
  ];
  nix.desktop = [
    # gpu
    ({
      lib,
      config,
      ...
    }: {
      nixpkgs.config.nvidia.acceptLicense = true;
      services.xserver.videoDrivers = ["nvidia"];
      hardware.nvidia = {
        modesetting.enable = true;
        package = config.boot.kernelPackages.nvidiaPackages.beta;
        videoAcceleration = true;
        open = false;
      };
      environment.variables =
        {
          LIBVA_DRIVER_NAME = "nvidia";
          NVD_BACKEND = "direct";
        }
        // lib.mkIf (config.programs.niri.enable == true) {
          GBM_BACKEND = "nvidia-drm";
          __GLX_VENDOR_LIBRARY_NAME = "nvidia";
        };
    })
    # tablet support
    {
      hardware.uinput.enable = true;
      programs.weylus.enable = true;
      services.udev.extraRules = ''
        KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
      '';
    }
  ];
}
