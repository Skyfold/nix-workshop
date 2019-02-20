let
  dash-haskell-rev = import (pkgs.fetchFromGitHub 
    { owner = "NixOS";
      repo = "nixpkgs";
      inherit 
       ({ url = "https://github.com/NixOS/nixpkgs.git";
          rev = "f7e7b814a95641e93543647c8777edaf16b17541";
          sha256 =
            "0x85r24pfsinlm6qwj4hmv7jizz4310b6sd96485g4c3qv6f9n42";
          fetchSubmodules = false;
	}) rev sha256;
    }) {};


  default = (import ./default.nix {});

  pkgs = default.pkgs;

  project-modified = default.project.env.overrideAttrs (
  old: rec {
    buildInputs = 
      [
        pkgs.haskellPackages.cabal-install 
        pkgs.haskellPackages.hpack
        pkgs.docker
        dash-haskell-rev.haskellPackages.dash-haskell
        # pkgs.haskellPackages.hlint
        # pkgs.haskellPackages.apply-refact
        # pkgs.haskellPackages.hindent
        # pkgs.haskellPackages.stylish-haskell
        # pkgs.haskellPackages.ghcid
        # pkgs.jq
        # pkgs.haskellPackages.threadscope
      ];
    }
  );
in
  project-modified
