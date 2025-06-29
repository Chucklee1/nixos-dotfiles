{
  nix.global = [
    (
      {
        pkgs,
        machine,
        extlib,
        ...
      }: {
        environment = let
          root = "$HOME/nixos-dotfiles";
          buildFlags = "--show-trace --impure";
          buildType = extlib.ifDarwin pkgs.system "darwin" "nixos";
        in {
          variables = {
            BASH_SILENCE_DEPRECATION_WARNING = "1";
            TERMINAL = "kitty";
            EDITOR = "nvim";
          };
          shellAliases = {
            y = "yazi";
            ny = "cd ${root} && yazi";
            update-flake = "nix flake update --flake ${root}";
            rebuild-flake = "sudo ${buildType}-rebuild switch --flake ${root}#${machine} ${buildFlags}";
          };
        };
      }
    )
  ];
  home.global = [
    ({pkgs, ...}: {
      programs = {
        git.userEmail = "kermitthefrog@kakao.com";
        git.userName = "Chucklee1";
        lazygit.settings.notARepository = "skip";
        lazygit.settings.promptToReturnFromSubprocess = false;
        oh-my-posh.useTheme = "pure";
        yazi.plugins = with pkgs; {
          bookmarks = fetchFromGitHub {
            owner = "dedukun";
            repo = "bookmarks.yazi";
            rev = "fbb7c00b6f887d5c0d78367bd4763ea8dff53459";
            hash = "sha256-Ry3V29T7lij5JR68gTINXtOEgbrYPwd5zQDEa2kfpTA=";
          };
          compress = fetchFromGitHub {
            owner = "KKV9";
            repo = "compress.yazi";
            rev = "9fc8fe0bd82e564f50eb98b95941118e7f681dc8";
            hash = "sha256-VKo4HmNp5LzOlOr+SwUXGx3WsLRUVTxE7RI7kIRKoVs=";
          };
          mediainfo = fetchFromGitHub {
            owner = "boydaihungst";
            repo = "mediainfo.yazi";
            rev = "b74b7b82aa468d8715547628511e4495df455790";
            hash = "sha256-RKSHos4Jnr6MYTtvzRvM2+c//9kyKD/pm1GGgrKcPc4=";
          };
          lazygit = fetchFromGitHub {
            owner = "Lil-Dank";
            repo = "lazygit.yazi";
            rev = "7a08a0988c2b7481d3f267f3bdc58080e6047e7d";
            hash = "sha256-OJJPgpSaUHYz8a9opVLCds+VZsK1B6T+pSRJyVgYNy8=";
          };
          restore = fetchFromGitHub {
            owner = "boydaihungst";
            repo = "restore.yazi";
            rev = "2c2802d8d2941dcf2c26bd07587de69e3589549c";
            hash = "sha256-n/PNwyCOL3uCWgtSjq4+dPwr+hWSF4h9fbvZOQPBuYM=";
          };
          mount = fetchFromGitHub {
            owner = "yazi-rs";
            repo = "plugins";
            rev = "2ad42fa7065b4885ff058280b4ab4309c11a5755";
            hash = "sha256-+FDiOaEYkA0A8w+Cg2Y/pFZY56lOsWLeKCNwLI0ZKVk=";
          };
        };
        # lib.genAttrs ["mediainfo" "mount" "restore"] (pn: pkgs.yaziPlugins.${pn});
        yazi.settings.theme.icon.prepend_dirs = [
          {
            name = "Modding";
            text = "";
          }
          {
            name = "nixos-dotfiles";
            text = "";
          }
          {
            name = "PrismLauncher";
            text = "";
          }
          {
            name = "Repos";
            text = "";
          }
          {
            name = "Server";
            text = "";
          }
        ];
      };
    })
  ];
}
