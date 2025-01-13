{
  def,
  inputs,
  ...
}: {
  imports = [
    ./nixos/dwm.nix
    ./nixos/system.nix
    ./nixos/virt.nix
    ./hardware/nvidia.nix
    ./hardware/amdgpu.nix
  ];

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {inherit inputs def;};
    users.${def.username}.home = {
      stateVersion = "24.05"; # DO NOT CHANGE
      username = "${def.username}";
      homeDirectory = "/home/${def.username}";
    };
    sharedModules = [
      ./home/shelli.nix
      ./home/nixvim.nix
      ./home/user-theming.nix
    ];
  };
}
