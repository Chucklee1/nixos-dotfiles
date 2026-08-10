{
  nix = [
    # fire exeptions
    ({lib, ...}: {
      networking.firewall.allowedTCPPorts = lib.mkAfter [
        4533
        6767
        8000
      ];
    })
    # cloudflared
    ({
      config,
      user,
      ...
    }: {
      sops.secrets."cloudflared/creds" = {
        owner = user;
        group = "users";
        mode = "0400";
      };
      sops.secrets."cloudflared/cert" = {
        owner = user;
        group = "users";
        mode = "0400";
      };

      services.cloudflared = {
        enable = true;
        tunnels."8ac6c5b7-7811-4248-b6c6-5d8a894fa5a1" = {
          default = "http_status:404";
          warp-routing.enabled = true;
          certificateFile = "${config.sops.secrets."cloudflared/cert".path}";
          credentialsFile = "${config.sops.secrets."cloudflared/creds".path}";
          ingress = {
            "chucklee.uk" = "https://chucklee-uk.cooperkang4.workers.dev"; # main site
            "navidrome.chucklee.uk" = "http://localhost:4533"; # navidrome
            "audiobookshelf.chucklee.uk" = "http://localhost:8000"; # audiobookshelf
          };
        };
      };
    })
    (let
      music_dir = "/srv/media/Music";
    in {
      services.nfs.server.exports = ''
        ${music_dir} nixos-desktop(ro,fsid=0,no_subtree_check)
        ${music_dir} nixos-laptop(ro,fsid=0,no_subtree_check)
        ${music_dir} goat-macbook(ro,fsid=0,no_subtree_check)
      '';
      services.navidrome = {
        enable = true;
        settings = {
          Address = "localhost";
          MusicFolder = music_dir;
          EnableTranscodingConfig = false;
          CoverArtQuality = 100;
          DefaultTheme = "Nord";
        };
      };
      services.audiobookshelf.enable = true;
    })
  ];
}
