{
  home = [
    {
      programs.git = {
        enable = true;
        settings.user.email = "kermitthefrog@kakao.com";
        settings.user.name = "Chucklee1";
      };
      programs.lazygit = {
        enable = true;
        settings.notARepository = "skip";
        settings.promptToReturnFromSubprocess = false;
      };
    }
  ];
}
