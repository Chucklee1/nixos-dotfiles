{
  nix = [
    {
      powerManagement.enable = true;
        services.upower = {
          enable = true;
          usePercentageForPolicy = true;
          percentageLow = 20;
          percentageCritical = 10;
          percentageAction = 5;
          criticalPowerAction = "Hibernate";
        };
    }
  ];
}
