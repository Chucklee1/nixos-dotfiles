{
  inputs,
  host,
  ...
}: {
  modules = [
    inputs.home-manager.nixosModules.home-manager
    inputs.stylix.nixosModules.stylix
    inputs.niri.nixosModules.niri
    inputs.grub2-themes.nixosModules.default
    ./hosts/${host}/default.nix
    ./shared/user.nix
    ./shared/system.nix
    ./shared/software.nix
    ./shared/theming.nix
    {
      home-manager.sharedModules = [
        inputs.nixvim.homeManagerModules.nixvim
        ./home/settings.nix
        ./home/theming.nix
        ./home/shelli.nix
        ./home/niri.nix
        ./home/nixvim.nix
        ./home/vscode.nix
        ./home/waybar.nix
      ];
    }
  ];
}
