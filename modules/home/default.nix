{
  imports = [
    ./nixvim
  ];

  nixpkgs.config.allowUnfree = true;

  # TODO: Set your username
  home = {
    username = "goat";
    homeDirectory = "/Users/goat";
  };

  programs.home-manager.enable = true;
  programs.git = {
    username = "goat-darwin";
    email = "kermitthefrog@kakao.com";
    enable = true;
  };

  # Nicely reload system units when changing configs
  #systemd.user.startServices = "sd-switch";

  home.stateVersion = "24.05";
}