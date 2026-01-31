{
  home = [
    {
      programs.git = {
        enable = true;
        settings = {
          user.email = "kermitthefrog@kakao.com";
          user.name = "Chucklee1";
          alias = {
            aa = "add -A";
            ce = "commit -e";
            fix = "commit --fixup HEAD";
            reword = "commit --amend";
            squash = "rebase --autosquash HEAD~2";
            unstage = "restore --staged";
            undo = "reset --soft HEAD~1";
            st = "status -sb";
            lg = "log --oneline --graph --decorate --all";
          };
        };
      };
      programs.lazygit = {
        enable = true;
        settings.notARepository = "skip";
        settings.promptToReturnFromSubprocess = false;
      };
    }
  ];
}
