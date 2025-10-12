{self, ...}: rec {
  inherit (self) inputs extlib;

  # module definition
  nixvimModule = system: profile: {
    # profile choices: core, full
    inherit system;
    module.imports = extlib.simpleMerge "${self}/modules/nixvim";
    extraSpecialArgs = {inherit inputs extlib profile;};
  };

  # output helpers
  mkModule = system: profile: inputs.nixvim.legacyPackages.${system}.makeNixvimWithModule (nixvimModule system profile);
}
