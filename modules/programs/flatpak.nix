{inputs, ...}: {
  nix = [
     inputs.nix-flatpak.nixosModules.nix-flatpak
     ({pkgs, ...}: {
       services.flatpak = {
         enable = true;
         packages = [
           rec {
             appId = "com.hypixel.HytaleLauncher";
             sha256 = "sha256-9lKXjb05cM/sOucUPbFmSLIsh8kItLl8V8Rou8ccJew=";
             bundle = "${pkgs.fetchurl {
               url = "https://launcher.hytale.com/builds/release/linux/amd64/hytale-launcher-latest.flatpak";
               inherit sha256;
             }}";
           }
         ];

       };
     })
  ];
}
