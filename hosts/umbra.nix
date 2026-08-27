{
  inputs,
  mod,
  ...
}:
with mod; {
  system = "x86_64-linux";
  type = "nixos";
  user = "goat";
  modules = [
    system.home
    system.pkgconfig
    system.sys-specs
    system.users

    shell.fish
    shell.variables

    services.ollama

    programs.git
    programs.yazi

    theming.stylix
  ];
  extraConfig = [
    "${inputs.nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
    # super safe stuff here
    {
      services.getty.autologinUser = "goat";
      security.sudo.wheelNeedsPassword = false;
      nix.settings = {
        substituters = [ "https://cache.nixos-cuda.org" ];
        trusted-public-keys = [ "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M=" ];
      };
    }
    {
      virtualisation.cores = 8;
      virtualisation.memorySize = 16384;
      virtualisation.graphics = false;
      virtualisation.sharedDirectories = {
        repos = {
          source = "/home/goat/Repos";
          target = "/home/goat/Repos";
        };
      };

      fileSystems."/var/lib/ollama" = {
        device = "/dev/vdb";
        fsType = "ext4";
      };

      virtualisation.qemu.options = [
        "-drive"
        "file=/var/lib/libvirt/images/nixos.img,format=raw,if=virtio,cache=none,aio=native"
        "-device vfio-pci,host=0a:00.0,multifunction=on"
        "-device vfio-pci,host=0a:00.1"
        "-netdev user,id=net0,hostfwd=tcp::2222-:22,hostfwd=tcp::11434-:11434"
        "-device virtio-net-pci,netdev=net0"
      ];

      services.openssh.enable = true;
      networking.firewall.allowedTCPPorts = [22];
      users.users.goat.openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF15qIVCXsm5U8+LD2lU3n3Ql9q8lwQ1FJuoh+MbW9SL goat@nixos-desktop"
      ];
    }
    # gpu/cuda stuff
    ({
      config,
      pkgs,
      ...
    }: {
      nixpkgs.config = {
        allowUnfree = true;
        cudaSupport = true;
      };

      # just in case
      boot.blacklistedKernelModules = ["nouveau"];

      hardware.graphics.enable = true;
      services.xserver.videoDrivers = ["nvidia"];
      hardware.nvidia = {
        branch = "production";
        package = config.boot.kernelPackages.nvidiaPackages.stable;
        open = false;
      };

      programs.nix-ld.enable = true;

      environment.systemPackages = with pkgs; [
        stdenv.cc
        binutils
        pciutils
        file
        cmake
        ninja
        gnumake
        gcc

        ffmpeg-full

        python314
        uv

        ffmpeg
        fmt.dev

        cudaPackages.cuda_cudart
        cudatoolkit
        cudaPackages.cudnn

        libGLU
        libGL
        libXi
        libXmu
        freeglut
        libXext
        libX11
        libXv
        libXrandr
        zlib
        ncurses
      ];

      environment.variables = {
        LD_LIBRARY_PATH = "${config.hardware.nvidia.package}/lib:$LD_LIBRARY_PATH";
        CUDA_PATH = "${pkgs.cudatoolkit}";
        EXTRA_LDFLAGS = "-L/lib -L${config.hardware.nvidia.package}/lib";
        EXTRA_CCFLAGS = "-I/usr/include";
        CMAKE_PREFIX_PATH = "${pkgs.fmt.dev}:$CMAKE_PREFIX_PATH";
        PKG_CONFIG_PATH = "${pkgs.fmt.dev}/lib/pkgconfig:$PKG_CONFIG_PATH";
      };
    })
  ];
}
