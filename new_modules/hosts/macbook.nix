{
  nix = [
    ({user, ...}: {
      # nix issue fix
      nix.settings.auto-optimise-store = false;

      # general
      system.stateVersion = 6;
      system.primaryUser = user;

      # user
      users.knownUsers = [user];
      users.users.${user} = {
        name = user;
        uid = 501;
        home = "/Users/${user}";
        ignoreShellProgramCheck = true;
      };
    })
    # symlink nix & home manager apps to /Applications
    # lets spotlight or dmenu-mac finally acess nix apps
    ({lib, config, user, ...}: {
      system.activationScripts.applications.text = lib.mkForce ''
        appDirPrev="${config.users.users.${user}.home}/Applications/Home Manager Apps"
        appDirFinal="/Applications"
        for i in "$appDirPrev"/*; do
          [ -e "$i" ] || continue
          dest="$appDirFinal/$(basename "$i")"

          # Remove existing app or alias
          if [ -e "$dest" ]; then
            rm -rf "$dest"
          fi

          # Copy new app/alias
          cp -R "$i" "$appDirFinal"/
        done
      '';
    })
    {
      # defaults
      system.defaults.WindowManager.StandardHideDesktopIcons = true;
      system.defaults.dock = {
        autohide = true;
        dashboard-in-overlay = true; # Don't show dashboard as a space
        mru-spaces = false; # Don't rearrange spaces based on most recently used
        show-recents = false; # don't show recent apps
        static-only = false; # show only running apps
        # Disable all hot corners
        wvous-tl-corner = 1;
        wvous-tr-corner = 1;
        wvous-bl-corner = 1;
        wvous-br-corner = 1;
      };

      system.defaults.finder = {
        AppleShowAllExtensions = true;
        AppleShowAllFiles = true;
        ShowPathbar = true;
      };

      # misc
      system.defaults.CustomUserPreferences = {
        "com.apple.desktopservices".DSDontWriteNetworkStores = true;
        "com.apple.desktopservices".DSDontWriteUSBStores = true;
        "com.apple.AdLib".allowApplePersonalizedAdvertising = false;
      };

      # Add ability to used TouchID for sudo authentication
      security.pam.services.sudo_local.touchIdAuth = true;
    }
  ];
}
