{
  nix = [
    ({config, pkgs, user, ...}: let
      ollama-pkg =
        if (config.hardware.nvidia.enabled) then pkgs.ollama-cuda
        else pkgs.ollama-vulkan;
    in {
      environment.systemPackages = [ollama-pkg];
      # must make custom serivce for ollama for more options
      systemd.user.services.ollama-local = let
        USER_HOME = config.users.users.${user}.home;
      in {
        description = "locally hosted ollama server";
        wantedBy = [ "default.target" ];
        after = [ "network.target" ];
        environment = {
          HOME = "${USER_HOME}";
          OLLAMA_MODELS = "${USER_HOME}/.ollama/models";
        };
        serviceConfig = {
          Type = "exec";
          ExecStart = "${ollama-pkg}/bin/ollama serve";
          WorkingDirectory = USER_HOME;
        };
      };
    })
  ];
  }
