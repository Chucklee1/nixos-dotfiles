{
  nix.global = [
    # firewall
    {
      networking.firewall = {
        enable = true;
        allowedTCPPorts = [22 80];
      };
    }
    # ssh
    {
      services.openssh = {
        enable = true;
        settings = {
          PasswordAuthentication = false;
          PermitRootLogin = "prohibit-password";
        };
      };
    }
    # tailscale
    {
      services.tailscale = {
        enable = true;
        port = 443;
        useRoutingFeatures = "server";
      };
    }
  ];
  nix.desktop = [
    ({
      lib,
      pkgs,
      ...
    }: let
      # helpers attrTemplates
      systemdService = name: desc: cfg: {
        name = {
          description = desc;
          after = ["network.target"];
          wantedBy = ["multi-user.target"];
          serviceConfig = {
            ExecStart = ''${lib.toLower pkgs.name}/bin/${lib.toLower pkgs.name} ${cfg}'';
            UMask = "0066";
          };
        };
      };

      duckService = prev: next: service: {
        "${service}.goat.duckdns.org" = {
          listen = [
            {
              addr = "0.0.0.0";
              port = next;
            }
          ];
          locations."/" = {
            proxyPass = "http://localhost:${prev}";
            extraConfig = ''
              proxy_set_header Host $host;
              proxy_set_header X-Real-IP $remote_addr;
            '';
          };
        };
      };

      # folders
      MEDIA = "/media/goat/BLUE_SATA/home/server/Media";
      ND = "/home/goat/server/Navidrome";
      ABS = "/media/goat/BLUE_SATA/home/server/AudioBookshelf";

      # navidrome cfg
      settings = (pkgs.formats.json {}).generate "config.json" {
        EnableInsightsCollector = false;
        MusicFolder = "${MEDIA}/Music";
        DataFolder = "${ND}/data";
        CacheFolder = "${ND}/cache";
      };
    in {
      systemd.services = lib.mergeAttrsList [
        (systemdService "Navidrome" "Navidrome Media Server"
          ''--configfile ${settings}'')
        (systemdService "Audiobookshelf" "AudioBookShelf audiobook server"
          ''--metadata ${ABS} --config ${ABS}'')
      ];

      services.nginx = {
        enable = true;
        virtualHosts = lib.mergeAttrsList [
          (duckService 4533 20 "navidrome")
          (duckService 13378 80 "audioBookShelf")
        ];
      };
      services.duckdns = {
        enable = true;
        domains = ["goat"];
        token = "your-duckdns-token";
        interval = 300;
      };
    })
  ];
}
