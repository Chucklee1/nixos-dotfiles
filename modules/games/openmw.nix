{
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = [
        pkgs.openmw
        (pkgs.momw-tools-pack.override {generateFishCompletions = true;})
      ];
    })
  ];
}
