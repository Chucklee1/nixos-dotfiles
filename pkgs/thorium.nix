{pkgs, ...}:
with pkgs;
  stdenv.mkDerivation rec {
    pname = "thorium-browser";
    version = "0";

    src = {
      depot_tools = builtins.fetchGit {
        url = "https://chromium.googlesource.com/chromium/tools/depot_tools.git";
        rev = "a7805fb7daaf664604d90f801f5d828593aab239";
      };
      thorium = builtins.fetchGit {
        url = "https://github.com/Alex313031/thorium.git";
        rev = "21f54da5e8ba83ca096be88998cd95f90ba29bdb";
        submodules = true;
      };
    };

    nativeBuildInputs = [ninja gn];

    buildPhase = ''
      # setup
      cd $out
      mkdir -p dev_tools chromium thorium
      cp -r ${src.depot_tools}/* depot_tools
      cp -r ${src.thorium}/* thorium

      export PATH="$out/depot_tools:$PATH"

      # chromium
      cd chromium
      fetch --nohooks chromium
      cd ./src
      ./build/install-build-deps.sh --no-nacl
      gclient runhooks
      cd $out

      #thorium
      cd thorium
      ./trunk.sh # rebase/sync chromium
      #./version.sh

      # install
      ./build.sh 4

      # cleanup
      clean.sh

    '';
  }
