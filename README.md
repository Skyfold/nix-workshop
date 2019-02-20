# Disclaimer

I am writing this on the 19th of February 2019. *Nix* and all its related tooling are being upgraded and improved as I write this. There is no grantee that the commands I explain will work the same in the future. You have been warned! However, there are many parts that I can grantee will work for as long as GitHub has has not been destroyed by Microsoft.

## Introduction (Audience)

This is intended for those who either wish to understand more about using *Nix* and have yet had trouble wading through the documentation on [](nixos.org) or those who have heard of *Nix's* god like ability to deal with complex and multi-language dependencies. Regardless of your background I will assume you are at the start of your journey into the not-so-neck-beard world of *Nix*, but have a solid background in at least one programming language and know your way around the basics of bash or fish or zsh or a more obscure shell.

## What is the problem *Nix* solves?

More precisely, which of the many problems that *Nix* solves am I going to cover in this tutorial:

- reproducible development environment and deployment including:
  - minimal Docker images
  - simple single virtual machine deployment

## Install Nix

The installation instructions are, [here](http://nixos.org/nix/).

## Breakdown of this repository

I've tried to structure each major example as a challenge with
provided solutions. The fact is, you will learn better if you figure
it out yourself (with lots of hints) and a solution to compare with.

- simple-example
  - If you have never used Nix, start here
  - Goes through setting up a simple derivation
  - Basic tools and use-cases
- haskell-webserver
  - Walks through
    - Setup Haskell development
    - complete deployment example
    - creating minimal docker images
- miscellaneous
  - Using Dhall "instead" of Nix

## Note

For those coming to the workshop this was written for or for anyone
who wants to use this to run a Nix workshop, please create a `tmp.nix`
with the following contents:

```
let
  tarball-1809 = fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz;
    sha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
  };

  pkgs = import tarball-1809 { };

in
  with pkgs;
  stdenv.mkDerivation {
    name = "tmp";
    buildInputs = [
      go
      haskellPackages.cabal-install 
      haskellPackages.hpack
      haskell.compiler.ghc822
      docker
      dhall
    ];
  }
```

Then run `nix-shell tmp.nix`. This will cache most of the derivations
you will need for this workshop. It will reduce the time you spend
waiting for your nix expressions to evaluate. After it completes it
will give you a bash shell, just exit the shell.
