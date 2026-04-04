{
  nix = [
    ({user, ...}: {
      hardware.uinput.enable = true;
      users.users.${user}.extraGroups = ["uinput"];
      programs.weylus.enable = true;
    })
  ];
}
