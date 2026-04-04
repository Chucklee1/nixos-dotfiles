{
  nix = [
    ({
      config,
      user,
      ...
    }: {
      users.users.${user}.shell = config.programs.fish.package;
      programs.fish = {
        enable = true;
        useBabelfish = true;
        promptInit =
          #fish
          ''
            # only show hostname on remote devices
            set host_prompt ""
            if set -q SSH_CONNECTION
              set host_prompt @(hostname)
            end

            # indicate when in nix dev env
            set shell_prefix ""
            if set -q IN_NIX_SHEll
              set shell_prefix nix-shell:
            end

            set user_prompt $shell_prefix$USER$host_prompt

            function new_pwd
              string replace "$HOME" "~" "$PWD"
            end

            function fish_prompt
              echo -s (set_color red) "┌─[" (set_color normal) \
              $user_prompt (set_color red) "] " (set_color normal) \
              (set_color magenta) (new_pwd) (set_color normal) \
              \n (set_color red) "└> " (set_color normal)
            end
          '';
        shellInit =
          #fish
          ''
            # disable greeting
            set -U fish_greeting ""
          '';
      };
    })
  ];

  home = [
    {
      programs.fish = {
        enable = true;
        functions = {
          sln =
            # fish
            ''
              function cprint
                      echo -s (set_color $argv[1]) $argv[2] (set_color normal)
              end

              if test (count $argv) -ne 2
                      echo "Usage: s[afe]ln SOURCE TARGET"
                      return 1
              end

              set target $argv[1]
              set dest $argv[2]

              # target dne
              if not test -e $target
                      cprint red "target to symlink not found"
                      return 1
              end

              # set symlink to have same name as target
              # when dest has trailing /, assume directory type
              set dest_dir $dest

              # set symlink to have different name from target
              # basename of dest is assumed to be non-directory type
              if not string match -q "*/" $dest
                      set dest_dir (dirname $dest)
              end

              # create dir if dne
              if not test -e $dest_dir
                      cprint yellow "destination folder not found, creating now"
                      mkdir -p $dest_dir
              end

              # force since this will be run at login
              cprint green "symlinking $target to $dest"
              ln -sf $target $dest
            '';
        };
        shellInitLast =
          # fish
          ''
            set -U fish_pager_color_description yellow
          '';
      };
    }
  ];
}
