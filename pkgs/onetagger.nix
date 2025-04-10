{pkgs ? import <nixpkgs> {}}:
pkgs.stdenv.mkDerivation {
  pname = "onetagger";
  version = "1.7.0";

  src = pkgs.fetchFromGitHub {
    owner = "Marekkon5";
    repo = "onetagger";
    rev = "v1.7.0";
    sha256 = "<sha256>";
  };

  nativeBuildInputs = with pkgs; [
    rustc
    cargo
    nodejs
    nodePackages.pnpm
    pkg-config
    lld
    makeWrapper
    libclang
  ];

  buildInputs = with pkgs; [
    alsa-lib
    openssl.dev
    gtk3
    webkitgtk_4_1
  ];

  buildPhase = ''
    cd client
    pnpm install
    pnpm run build
    cd ..

    echo "Building backend..."
    cargo build --release
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp target/release/onetagger $out/bin/
  '';

  meta = with pkgs.lib; {
    description = "Cross-platform music tagger for DJs";
    homepage = "https://github.com/Marekkon5/onetagger";
    license = licenses.gpl3Plus;
    platforms = platforms.linux;
  };
}
