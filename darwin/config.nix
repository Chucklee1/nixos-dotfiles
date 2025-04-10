{pkgs, ...}: {
  nixpkgs = {
    hostPlatform = "x86-64-darwin";
    config.allowUnfree = true;
  };
  nix.settings.experimental-features = ["nix-command" "flakes"];
  environmnent.systemPackages = with pkgs; [neovim git];
}
