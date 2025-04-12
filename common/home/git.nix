{ops, ...}: {
  programs.git = {
    enable = true;
    userEmail = "${ops.userEmail}";
    userName = "${ops.userName}";
  };
}
