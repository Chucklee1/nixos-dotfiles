{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.amdcpu;
in {
  options.amdcpu = {
    enable = lib.mkEnableOption "enable amdcpu microcode and virtualization settings";
  };

  config = lib.mkIf cfg.enable {
    boot.kernelModules = ["kvm-amd"];
    hardware.cpu.amd.updateMicrocode = true;
  };
}
