{profile, ...}: {
  services.nixvim = (
    if profile == "core"
    then {
      telescope.enable = true;
    }
    else if (profile == "full")
    then {
      cmp.enable = true;
      telescope.enable = true;
      telescope.nerdIconLookup = true;
    }
    else {}
  );
}
