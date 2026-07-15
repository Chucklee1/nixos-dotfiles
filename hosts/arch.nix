{
  mod,
  self,
  ...
}:
with mod; {
  system = "x86_64-linux";
  type = "home";
  user = "goat";
  modules = [
    programs.git
    programs.kitty
    programs.niri
    programs.waybar
    programs.yazi

    shell.fish
    software.qol

    system.pkgconfig

    theming.stylix
  ];
  extraConfig = [
    # overlays
    {
      nixpkgs.overlays = [
        (import self.inputs.emacs-overlay)
        self.overlays.emacs
        self.overlays.nixvim
      ];
    }
    # base hm stuff
    ({
      pkgs,
      user,
      ...
    }: {
      home.username = user;
      home.homeDirectory = "/home/${user}";
      home.stateVersion = "26.05";
      home.sessionVariables = {
        BROWSER = "zen-browser";
        EDITOR = "nvim";
      };
      home.packages = [pkgs.nixvim];
      programs.home-manager.enable = true;
    })
    # target machine for hm to work properly + nixgl
    {
      targets.genericLinux = {
        enable = true;
        nixGL.defaultWrapper = "mesa";
        nixGL.vulkan.enable = true;
      };
    }
    # niri settings from nixos-desktop
    ({lib, ...}: {
      programs.niri.settings = {
        # must use {} since niri does not like "key = function -float;"
        input.mouse = lib.mkForce {accel-speed = -0.75;};
        # no clue why my monitor has so many 0's...
        outputs = {
          "HKC OVERSEAS LIMITED 24E4 0000000000001" = {
            mode = {
              width = 1920;
              height = 1080;
              refresh = 165.001;
            };
          };
        };
      };
    })
  ];
}
