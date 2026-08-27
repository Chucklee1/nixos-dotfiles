{
  nix = [
    {networking.firewall.allowedUDPPorts = [25565];}
    ({pkgs, ...}: {
      # mc server
      systemd.user.services.gtnh-server = let
        flags = [
          "-Xms4G"
          "-Xmx6G"
          "-XX:+UseCompactObjectHeaders"
          "-XX:MaxHeapFreeRatio=40"
          "-XX:MinHeapFreeRatio=10"
          "-Dfml.readTimeout=180"
          "-Dfml.queryResult=confirm" # fml
          "@java9args.txt "
          "-jar lwjgl3ify-forgePatches.jar "
          "nogui"
        ];
      in {
        enable = true;
        after = ["network.target"];
        wantedBy = ["multi-user.target"];
        description = "Server for GTNH 2.8.4 Java-25";
        serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.temurin-bin-25}/bin/java ${builtins.concatStringsSep " " flags}";
          WorkingDirectory = "/srv/Minecraft/GTNH";
          Restart = "on-failure";
        };
      };
    })
  ];
}
