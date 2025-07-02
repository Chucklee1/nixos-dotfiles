{
  description = "Never let them know your next move";

  # ---- main pkg providers ----
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.home-manager.url = "github:nix-community/home-manager";
  inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";

  # ---- disk formatting ----
  inputs.disko.url = "github:nix-community/disko";
  inputs.disko.inputs.nixpkgs.follows = "nixpkgs";

  # ---- macos ----
  inputs.nix-darwin.url = "github:LnL7/nix-darwin/master";
  inputs.nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

  # ---- theming ----
  inputs.stylix.url = "github:danth/stylix";
  inputs.minegrub-theme.url = "github:Lxtharia/minegrub-theme";
  inputs.minesddm.url = "github:Davi-S/sddm-theme-minesddm";
  inputs.minesddm.inputs.nixpkgs.follows = "nixpkgs";

  # ---- neovim ----
  inputs.nix-vim.url = "github:Chucklee1/nix-vim";

  # ---- wayland ----
  inputs.niri.url = "github:sodiboo/niri-flake";
  inputs.waybar.url = "github:Alexays/Waybar/master";

  outputs = self: nixpkgs: inputs: (import ./flake/output.nix {inherit self nixpkgs inputs;});
}
