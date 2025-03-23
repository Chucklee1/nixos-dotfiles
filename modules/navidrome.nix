{
  nix.laptop = [
    ({config, ...}: {
      services = {
        tailscale = {
          enable = true;
          port = 3030;
          useRoutingFeatures = "server";
        };
        navidrome = {
          enable = true;
          openFirewall = true;
          settings = {
            address = "localhost";
            MusicFolder = "/run/media/goat/T7/music";
          };
        };
      };
    })
  ];
}
