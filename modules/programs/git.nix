{
  home = [
    {
      programs.git = {
        enable = true;
        userEmail = "kermitthefrog@kakao.com";
        userName = "Chucklee1";
      };
      programs.lazygit = {
        enable = true;
        settings.notARepository = "skip";
        settings.promptToReturnFromSubprocess = false;
      };
    }
  ];
}
