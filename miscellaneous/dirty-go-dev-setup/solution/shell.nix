# Note, you will need to move this up a directory
# then run nix-shell

let
  pkgs = import <nixpkgs> {};

in
  with pkgs;
  stdenv.mkDerivation {
    name = "go-env";
    buildInputs = [
      go
    ];
    shellHook = ''
      export GOPATH=$(pwd);
      export PATH=$GOPATH/bin:$PATH
    '';
  }
