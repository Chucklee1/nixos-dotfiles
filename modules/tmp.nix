{
  merge,
  configs,
  ...
}: {
  laptop = merge configs.global configs.laptop;
  desktop = merge configs.global configs.desktop;
}
