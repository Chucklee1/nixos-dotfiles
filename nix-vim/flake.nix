{
  description = "Personal config for nixvim";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixvim.url = "github:nix-community/nixvim";
    customLib.url = "github:Chucklee1/nixos-dotfiles";
    en_us-dictionary.url = "github:dwyl/english-words";
    en_us-dictionary.flake = false;
  };

  outputs = {self, ...} @ inputs: let
    # function definitions are under the repo's root at libs.nix
    extlib = inputs.customLib.extlib;

    # module definition
    nixvimModule = system: profile: {
      # profile choices: core, full
      inherit system;
      module.imports = extlib.simpleMerge "${self}/config";
      extraSpecialArgs = {inherit inputs extlib profile;};
    };

    # output helpers
    mkCheck = system: profile: inputs.nixvim.lib.${system}.check.mkTestDerivationFromNixvimModule (nixvimModule system profile);
    mkModule = system: profile: inputs.nixvim.legacyPackages.${system}.makeNixvimWithModule (nixvimModule system profile);
  in {
    # using core and default  with core profile so one can either run core as default or directly state core
    checks = extlib.allSystems (system: {
      default = mkCheck system "core";
      core = mkCheck system "core";
      full = mkCheck system "full";
    });
    overlays.default = self: prev: {
      nixvim.default = mkModule self.system "core";
      nixvim.core = mkModule self.system "core";
      nixvim.full = mkModule self.system "full";
    };
    packages = extlib.allSystems (system: {
      default = mkModule system "core";
      core = mkModule system "core";
      full = mkModule system "full";
    });
  };
}
