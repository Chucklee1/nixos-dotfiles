{imports, ...}: {
  stylix.targets.grub.enable = false;
  boot.loader.grub2-theme = {
    enable = true;
    theme = "stylish";
    footer = true;
  };
}
