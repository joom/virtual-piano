let drv = import ./default.nix;
in if builtins.getEnv "IN_NIX_SHELL" != "" then drv.env else drv
