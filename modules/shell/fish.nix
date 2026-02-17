{
  nix = [
    ({
      config,
      user,
      ...
    }: {
      users.users.${user}.shell = config.programs.fish.package;
      programs.fish = {
        enable = true;
      };
    })
  ];

  home = [
    {
      programs.fish = {
        enable = true;
      };
    }
  ];
}
