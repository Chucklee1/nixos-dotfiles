{...}: {
  system.stateVersion = "24.05"; # DO NOT CHANGE
  networking.hostName = "goat";
  networking.networkmanager.enable = true;
  i18n.defaultLocale = "en_CA.UTF-8";
  services.automatic-timezoned.enable = true;
  # timedatectl list-timezones
  # sudo timedatectl set-timezone <timezone> (no "")
  console = {
    earlySetup = true;
    keyMap = "us";
  };
}
