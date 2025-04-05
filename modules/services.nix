{
  nix.global = [
    {
      /*
      device/internal services support
      */

      # audio
      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
      };

      # printing/pdf
      services.printing.enable = true;

      # disk optimizaition
      services.fstrim.enable = true;

      /*
      connectivity/network
      */

      # bluetooth
      hardware = {
        bluetooth.enable = true;
        bluetooth.powerOnBoot = true;
      };
      services.blueman.enable = true;

      # openssh
      services.openssh = {
        enable = true;
        settings = {
          PasswordAuthentication = false;
          PermitRootLogin = "prohibit-password";
        };
      };

      /*
      software
      */

      # thunar qol
      services = {
        tumbler.enable = true;
        gvfs.enable = true;
      };

      # display manager
      services.displayManager.ly.enable = true;

      # auto-timezone:
      #services.automatic-timezoned.enable = true;
      time.timeZone = "America/Vancouver";
    }
  ];

  nix.desktop = [
    # tablet support
    {
      users.users."goat".extraGroups = ["uinput"];
      hardware.uinput.enable = true;
      programs.weylus.enable = true;
      services.udev.extraRules = ''
        KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
      '';
    }
  ];
}
