let
  config = rec {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackgesOld: rec {

          webserver = pkgs.haskell.lib.justStaticExecutables
            ( haskellPackagesNew.callCabal2nix "webserver" ./. { });
        };
      };
    };
  };

  # pkgs = import <nixpkgs> { inherit config; system = "x86_64-linux"; };
  tarball-1809 = fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz;
    sha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
  };

  pkgs = import tarball-1809 { inherit config; system = "x86_64-linux"; };

in
  { webserver = pkgs.haskellPackages.webserver;
  }
