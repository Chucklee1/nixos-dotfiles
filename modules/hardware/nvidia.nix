{
  nix = [
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
