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
          function fish_prompt
            echo -s (set_color red) "┌─[" (set_color normal) \
            $USER (set_color red) "] " (set_color normal) \
            (set_color magenta) $(prompt_pwd) (set_color normal) \
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
        # have to initilize after shell theme
        shellInitLast = 
        # fish 
        ''
          set -U fish_pager_color_description yellow
        '';
      };
    }
  ];
}
