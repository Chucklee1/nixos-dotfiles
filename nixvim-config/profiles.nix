{profile, ...}: {
  services.nixvim = (
    if profile == "core"
    then {
      formatting.enable = false;
      telescope.enable = true;
    }
    else if (profile == "full")
    then {
      cmp.enable = true;
      formatting.enable = true;
      formatting.autoFormat = false;
      latex.enable = true;
      telescope.enable = true;
      telescope.nerdIconLookup = true;
    }
    else {}
  );
}
