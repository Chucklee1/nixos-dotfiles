{
  # must be nix level for sops
  home = [
    ({pkgs, ...}: {
      programs.discord.enable = true;
      programs.discord.package = pkgs.discord-canary;
    })
  ];
}
