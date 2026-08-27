{
  # must be nix level for sops
  nix = [
    ({
      config,
      pkgs,
      user,
      ...
    }: {
      home-manager.users.${user} = {
        programs.discord.enable = true;
        programs.discord.package = pkgs.discord-canary;
        home.packages = [
          (pkgs.writeShellScriptBin "discord-with-rpc" ''
            ${config.programs.discord.package} &
            ${pkgs.music-discord-rpc}/bin/music-discord-rpc" \
              --lastfm-api-key $(cat ${config.sops.secrets."api/lastfm".path})
          '')
        ];
      };
    })
  ];
}
