{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    niri.url = "github:sodiboo/niri-flake";
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    grub2-themes.url = "github:vinceliuice/grub2-themes";
  };

  outputs = {nixpkgs, ...} @ inputs: let
    # use nix-prefetch-url to get hash
    def = {
      username = "goat";
      system = "x86_64-linux";
      layout = "us";
      wallpaper = ./assets/wallpaper.png;
    };
    lib = nixpkgs.lib;
    /*
      importNixFiles = path: suffix: (
      lib.filter
      (n: lib.strings.hasSuffix "${suffix}" n)
      (lib.filesystem.listFilesRecursive ./modules/${path})
    );
    */
    readModulesRecursively = dir: suffix: let
      # List all entries in the directory
      entries = builtins.readDir dir;

      # Recursively handle directories and files
      processEntry = name: let
        path = "${dir}/${name}";
        info = builtins.readDir dir.${name};
      in
        if info.type == "directory"
        then
          # Recursively read directories
          readModulesRecursively path suffix
        else if lib.strings.hasSuffix suffix path
        then
          # Return valid files with matching suffix
          [(import path)]
        else []; # Ignore other files
    in
      # Flatten the results
      lib.flatten (map processEntry entries);

    # system declaration
    systemConfig = host: (nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs def host;};
      modules =
        (readModulesRecursively ./modules/shared ".mod.nix")
        ++ (readModulesRecursively ./modules/hosts/${host} ".nix")
        ++ [
          inputs.home-manager.nixosModules.home-manager
          inputs.stylix.nixosModules.stylix
          inputs.niri.nixosModules.niri
          inputs.grub2-themes.nixosModules.default
          {
            home-manager.sharedModules =
              (readModulesRecursively "./modules/shared" ".home.nix")
              ++ [inputs.nixvim.homeManagerModules.nixvim];
          }
        ];
    });
  in {
    # systems
    nixosConfigurations.desktop = systemConfig "desktop";
    nixosConfigurations.laptop = systemConfig "laptop";
  };
}
