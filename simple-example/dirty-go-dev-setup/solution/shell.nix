# Note, you will need to move this up a directory
# then run nix-shell

let
  tarball-1809 = fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz;
    sha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
  };

  pkgs = import tarball-1809 { };

in
  with pkgs;
  stdenv.mkDerivation {
    name = "go-env";
    buildInputs = [
      go
    ];
    shellHook = ''
      export GOPATH=$(pwd);
    '';
  }
