{nix = [({npkgs, ...}: {environment.systemPackages = [(npkgs.osu-lazer-bin.override {nativeWayland = true;})];})];}
