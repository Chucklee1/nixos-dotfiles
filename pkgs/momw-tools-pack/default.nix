{
  lib,
  appimageTools,
  fetchzip,
  makeWrapper,
  stdenv,
  tes3cmd,
  _7zip-zstd-rar,
  generateFishCompletions ? false,
  ...
}: let
  safeSystem = stdenv.hostPlatform.system
    or (throw "Error: Unsupported system");

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
      safeSystem
    };

  # to match naming scheme of momw tools
  arch =
    {
      aarch64-linux = "linux-arm64";
      x86_64-linux = "linux-amm64";
      aarch64-darwin = "macos-arm64";
      x86_64-darwin = "macos-amm64";
    }.${
      safeSystem
    };

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

    nativeBuildInputs = [ makeWrapper ];

    buildPhase = false;
    configurePhase = false;

    installPhase = ''
      mkdir -p $out/bin
      mkdir -p $out/share/doc/momw-tools-pack

      cp -r $src/* $out/bin
      chmod -R u+w $out/bin

      if [ -d $out/bin/Readmes ]; then
        mv $out/bin/Readmes/* $out/share/doc/momw-tools-pack
        rmdir $out/bin/Readmes
      fi

      mv $out/bin/version.txt $out/share/doc/momw-tools-pack

      if [ -f $out/bin/umo ]; then
        rm $out/bin/umo
      fi

      cp ${umoWrapped}/bin/umo $out/bin/

      if [ -f $out/bin/tes3cmd ]; then
        rm $out/bin/tes3cmd
      fi
      cp ${tes3cmd}/bin/tes3cmd $out/bin/
    '';

    postInstall = lib.optionalString generateFishCompletions ''
      export HOME=$TMPDIR/home
      mkdir -p "$HOME/.config/fish/completions"

      $src/momw-configurator-${arch} completions || true

      mkdir -p $out/share/fish/vendor_completions.d
      cp "$HOME/.config/fish/completions/"*.fish \
        $out/share/fish/vendor_completions.d/ || true
    '';

    postFixup = ''
      wrapProgram $out/bin/umo \
        --prefix PATH : ${lib.makeBinPath [
          _7zip-zstd-rar
        ]}
    '';
  }
