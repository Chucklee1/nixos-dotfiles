{fetchgit}: {final: prev: {
  dwm = prev.dwm.overrideAttrs (old: {
    src = fetchgit {
      url = "https://github.com/Chucklee1/dwm";
      sparseCheckout = ["dwm"];
      rev = "main";
      hash = "sha256-1cdbaf1894fbb5141181c1401897a9288999445a=";
    };
  });
  slstatus = prev.slstatus.overrideAttrs (old: {
    src = fetchgit {
      url = "https://github.com/Chucklee1/dwm";
      sparseCheckout = ["slstatus"];
      rev = "main";
      hash = "sha256-1cdbaf1894fbb5141181c1401897a9288999445a=";
    };
  });
}
