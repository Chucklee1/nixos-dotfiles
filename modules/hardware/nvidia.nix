{
  nix = [
    {
      nix.settings = {
        substituters = [ "https://cache.nixos-cuda.org" ];
        trusted-public-keys = [
          "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="
        ];
      };
    }
    {
      services.xserver.videoDrivers = ["nvidia"];
      hardware.nvidia = {
        modesetting.enable = true;
        powerManagement.enable = false;
        powerManagement.finegrained = false;
        open = false;
      };
    }
    # software overrides to compensate for nvidia...
    ({pkgs, ...}: {
      services.sunshine.package = pkgs.sunshine.override {
        cudaSupport = true;
        cudaPackages = pkgs.cudaPackages;
      };
    })
  ];
}
