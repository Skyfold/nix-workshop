{compiler ? "ghc822"}:

let 
  tarball-1809 = fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz;
    sha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
  };

  pkgs = import tarball-1809 { };

  project = pkgs.haskell.packages.${compiler}.callCabal2nix "webserver" ./. { };
in
  # with pkgs.haskell.packages.${compiler};
  { inherit project; inherit pkgs; }
