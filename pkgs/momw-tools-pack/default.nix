{
  appimageTools,
  stdenv,
  fetchzip,
  ...
}: let
  source =
    rec {
      aarch64-linux = fetchzip {
        url = "https://gitlab.com/modding-openmw/momw-tools-pack/-/package_files/306059120/download";
        extension = "tar.gz";
        hash = "sha256-3uF+EzjpuFZqbek/A09W9CIG5ofbXDJVppwew2I7gsU=";
      };
      x86_64-linux = aarch64-linux;

      aarch64-darwin = fetchzip {
        url = "https://gitlab.com/modding-openmw/momw-tools-pack/-/package_files/306059134/download";
        extension = "tar.gz";
        hash = "sha256-D24w7Ne3dsY771WaZzjlBJVS4NfQrDbUgaAuk8k+/NE=";
      };
      x86_64-darwin = aarch64-darwin;
    }.${
      stdenv.hostPlatform.system
    } or (throw "Unsupported system");

  umoWrapped = appimageTools.wrapType2 {
    pname = "umo";
    version = "6.7";
    src = "${source}/umo";
  };
in
  stdenv.mkDerivation {
    pname = "momw-tools-pack";
    version = "1.48";
    src = source;

    buildPhase = false;
    configurePhase = false;

    installPhase = ''
      mkdir -p $out/bin
      mkdir -p $out/share/doc/momw-tools-pack

      cp -r $src/* $out/bin
      chmod -R u+w $out/bin
      mv $out/bin/Readmes/* $out/share/doc/momw-tools-pack
      mv $out/bin/version.txt $out/share/doc/momw-tools-pack
      rmdir $out/bin/Readmes
    '';

    fixupPhase = ''
      rm $out/bin/umo
      cp ${umoWrapped}/bin/umo $out/bin/
    '';
  }
