# Disclaimer

I am writing this on the 29th of September 2018. *Nix* and all its related tooling are being upgraded and improved as I write this. There is no grantee that the commands I explain will work the same in the future. You have been warned! However, there are many parts that I can grantee will work for as long as GitHub has has not been destroyed by Microsoft. I will *not* cover how to keep your derivation building regardless of what happens to GitHub, but all that requires if for you to keep a copy of all build dependencies' source code.

## Introduction (Audience)

This is intended for those who either wish to understand more about using *Nix* and have yet had trouble wading through the documentation on [](nixos.org) or those who have heard of *Nix's* god like ability to deal with complex and multi-language dependencies. Regardless of your background I will assume you are at the start of your journey into the not-so-neck-beard world of *Nix*, but have a solid background in at least one programming language and know your way around the basics of bash or fish or zsh or a more obscure shell.

## What is the problem *Nix* solves?

More precisely, which of the many problems that *Nix* solves am I going to cover in this tutorial:

- reproducible development environment including all tools necessary for the project

## Install Nix

The installation instructions are, [here](http://nixos.org/nix/).

## Example 1, managing a Kubernetes cluster

Typically Nix is touted as the best way to manage programming language dependencies, but in reality the need for a specialized environment goes far beyond code dependencies. At a glance Kubernetes only needs `kubectl` and some authentication credentials, but in reality you actually need a specific version of `kubectl` and `helm` for that cluster as well as `openssl`, `telepresence` and maybe `jq` or `jp`. Plus since this cluster is on Google you need `gcloud` and you need `cloud_sql-proxy` to work with the Postgresql database you have on Google. This means you probably want `psql` with a specific version and maybe `dbeaver` to manage the database. Ah and to test changes locally you might want `docker` as well. Now you either create a single virtual machine that is entirely responsible for this one cluster or you hope all these tools will be compatible with any other cluster you spin up and no other project you work on accidentally changes these tools. In an ideal world you would have none of these tools installed globally and only accessible the moment you `cd kubernetes-project-directory`. This is what I'm going to show you how to do.

### Start Small

Lets start with pulling in the above tools in a bash shell without specifying any versions:

```
nix-shell -p kubernetes-helm kubernetes docker google-cloud-sdk openssl postgresql100 jq jp
```

This gives you a bash shell with the tools these packages provide in your path and brings in anything you already have in your environment (like `vim`). Note, I removed `telepresence` because the version in the unstable NixPkgs set fails to build. Later on we will add it back in.

Now, lets turn this command into a file, create a file called `shell.nix` with these contents (if you cloned this repository `cd simple-example`):

```
let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell { buildInputs = with pkgs; [ kubernetes-helm kubernetes
                                            docker google-cloud-sdk openssl postgresql100 jq jp]; }
```

Then call `nix-shell` in the directory with the `shell.nix`. It should come up immediately since with the previous command you already downloaded or built all the tools. See [this link](https://nixos.org/nixos/nix-pills/basics-of-language.html) for an introduction of the Nix language, just ignore the instruction to install `nix-repl` which is now bundled by default as `nix repl`. The magic is the "import <nixpkgs> {};"


### Approaching Reproducibility 



### Usability

Note: You will need to setup `direnv` if you want your shell to bring up the environment for you, [see direnv github page](https://github.com/direnv/direnv/wiki/Nix), instead of calling `nix-shell` manually.

Another file called `.envrc` with:

```
use_nix
```

