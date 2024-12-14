{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    niri.url = "github:sodiboo/niri-flake";
    grub2-themes.url = "github:vinceliuice/grub2-themes";
  };

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
    wallpaper = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/Chucklee1/assets/refs/heads/main/elqlyrb492u71.PNG";
      sha256 = "0c16zcn5sfq704hi6s0ia200fjdnn2q5yra9hccpqxzrkf4l1lsi";
    };
    specialArgs = {inherit inputs system wallpaper;};
    shared-modules = with inputs;
      [
        home-manager.nixosModules.home-manager
        stylix.nixosModules.stylix
        niri.nixosModules.niri
        grub2-themes.nixosModules.default
      ]
      ++ [./modules/default.nix];
  in {
    # -----------------------------------------------------------
    # desktop profile - WIP
    # -----------------------------------------------------------

    # -----------------------------------------------------------
    # laptop profile
    # -----------------------------------------------------------
    nixosConfigurations.laptop = nixpkgs.lib.nixosSystem {
      system = system;
      specialArgs = specialArgs;
      modules =
        shared-modules
        ++ [
          ./modules/machines/laptop.nix
          {
            vscode.enable = true;
            thunar.enable = false;
            niri.enable = true;
            theme-minimal.enable = true;
            theme-fancy.enable = false;
            AMD.enable = true;
            nvidia.enable = false;
            radeon.enable = true;
          }
          0
        ];
    };
    # -----------------------------------------------------------
    # lazy-insaller script
    # -----------------------------------------------------------
    packages.x86_64-linux.lazy-installer = let
      name = "lazy-installer";
      script = pkgs.writeShellScriptBin name ''
        ${pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/Chucklee1/nixos-dotfiles/main/lazy-installer.sh"; # Replace with correct URL
          sha256 = "00rkgmiv1lmhr3agjxalaf5zyxjalbk2zvddav6nh4a2klsfdfp8"; # Correct SHA256 hash
        }} "$@"
      '';
    in
      pkgs.symlinkJoin {
        name = name;
        paths = [script] ++ [pkgs.parted pkgs.git];
        buildInputs = [pkgs.makeWrapper];
        postBuild = "wrapProgram $out/bin/${name} --prefix PATH : $out/bin";
      };
  };
}
