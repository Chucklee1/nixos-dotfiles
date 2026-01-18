{inputs, ...}: {
  home = [
    ({pkgs, ...}: {
      home.packages = [
        (inputs.quickshell.packages.${pkgs.stdenv.hostPlatform.system}.default.withModules [
          inputs.qml-niri.packages.${pkgs.stdenv.hostPlatform.system}.default
        ])
      ];
    })
  ];
}
