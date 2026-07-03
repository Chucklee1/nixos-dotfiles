{self, ...}: let
  inherit (self) inputs extlib;
  mkModule = system:
    inputs.nixvim.legacyPackages.${system}.makeNixvimWithModule
    {
      inherit system;
      module.imports = extlib.simpleMerge ./config;
      extraSpecialArgs = {inherit inputs extlib;};
    };
in
  final: prev: {
    nixvim = mkModule final.system;
  }
