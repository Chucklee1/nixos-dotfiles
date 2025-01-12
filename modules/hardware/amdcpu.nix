{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.amdcpu;
in {
  options.amdgpu = {
    enable = lib.mkEnableOption "enables drivers for amd cpus";
    default = false;
  };

  config = lib.mkIf cfg.enable {
    boot.kernelModules = ["kvm-amd"];
    hardware.cpu.amd.updateMicrocode = true;
  };
}
