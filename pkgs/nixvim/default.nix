{self, ...}: let
  inherit (self) inputs extlib;
  mkModule = system:
    inputs.nixvim.legacyPackages.${system}.makeNixvimWithModule
    {
      # profile choices: core, full
      inherit system;
      module.imports = extlib.simpleMerge ./config;
      extraSpecialArgs = {inherit inputs extlib;};
    };
in {
  overlay = final: prev: {
    nixvim= mkModule final.system;
  };

  package = system: mkModule system;
}
