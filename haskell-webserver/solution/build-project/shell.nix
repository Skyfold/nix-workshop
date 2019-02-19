let
  default = (import ./default.nix {});

  pkgs = default.pkgs;

  project-modified = default.project.env.overrideAttrs (
  old: rec {
    buildInputs = 
      [
        pkgs.haskellPackages.cabal-install 
        pkgs.haskellPackages.hpack
        pkgs.docker
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
