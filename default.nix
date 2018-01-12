# default.nix
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    app = ./app;
  };

  shells = {
    ghc = ["app"];
    ghcjs = ["app"];
  };

  withHoogle = false;
})
