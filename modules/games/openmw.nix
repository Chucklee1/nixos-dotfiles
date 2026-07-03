{self, ...}: {
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = [
        self.packages.${pkgs.stdenv.hostPlatform.system}.openmw
        (self.packages.${pkgs.stdenv.hostPlatform.system}.momw-tools-pack.override {generateFishCompletions = true;})
      ];
    })
  ];
}
