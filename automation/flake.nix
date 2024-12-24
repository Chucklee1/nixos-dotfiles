{
  description = "lazy scripts";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = {nixpkgs, ...}: let
    # nixpkgs
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
    # script
    packages.${system}.lazy-installer = {
      name = "lazy-installer";
      script = pkgs.writeShellScriptBin name ''
        git clone https://github.com/Chucklee1/nixos-dotfiles
        sleep 1
        cd nixos-dotfiles
        ./lazy-installer.sh "$@"
      '';
    };
  in
    pkgs.symlinkJoin {
      name = {inherit name;};
      paths = [script] ++ [pkgs.parted pkgs.git];
      buildInputs = [pkgs.makeWrapper];
      postBuild = "wrapProgram $out/bin/${name} --prefix PATH : $out/bin";
    };
}
