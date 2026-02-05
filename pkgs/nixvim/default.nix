{self, ...}: let
  inherit (self) inputs extlib;
  mkModule = system: profile:
    inputs.nixvim.legacyPackages.${system}.makeNixvimWithModule
    {
      # profile choices: core, full
      inherit system;
      module.imports = extlib.simpleMerge ./config;
      extraSpecialArgs = {inherit inputs extlib profile;};
    };
in {
  overlay = final: prev: {
    nixvim.core = mkModule final.system "core";
    nixvim.full = mkModule final.system "full";
  };

  package = system: {
    core = mkModule system "core";
    full = mkModule system "full";
  };
}
