{...}: {
  # sym linking
  home.file.".config/niri/config.kdl".source = ../../home/niri.kdl;

  home.packages = with pkgs; [
    swaylock-effects
    swayidle
    swww
  ];

  programs = {
    lazygit.enable = true;
    fuzzel.enable = true;
    wlogout.enable = true;
  };
}
