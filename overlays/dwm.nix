{fetchgit}: {final: prev: {
  dwm = prev.dwm.overrideAttrs (old: {
    src = fetchgit {
      url = "https://github.com/Chucklee1/dwm";
      rev = "main";
      hash = "sha256-19zcqpaxnkbmwr5v8pv4rrgj9bd641qiiygkvvc4dnzh9wp5mxs5=";
    };
  });
  slstatus = prev.slstatus.overrideAttrs (old: {
    src = fetchgit {
      url = "https://github.com/Chucklee1/dwm";
      rev = "main";
      hash = "sha256-00nly3wc38as7lfir097safapj1xlqrvq1ycqdc8bw51102s96mh=";
    };
  });
}
