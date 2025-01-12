{
  config,
  lib,
  pkgs,
  host,
  ...
}: {
  config =
    lib.mkIf ("${host}" == "desktop" || "${host}" == "laptop")
    {
      boot.kernelModules = ["kvm-amd"];
      hardware.cpu.amd.updateMicrocode = true;
    };
}
