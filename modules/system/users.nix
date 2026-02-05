{
  nix = [
    {
      users.users.goat = {
        name = "goat";
        isNormalUser = true;
        extraGroups = [
          "media"
          "wheel"
          "input"
          "networkmanager"
          "gamemode"
          "libvirtd"
        ];
      };
    }
  ];
}
