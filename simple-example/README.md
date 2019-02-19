# Intro

Typically Nix is touted as the best way to manage programming language dependencies, but in reality the need for a specialized environment goes far beyond code dependencies. Here are a nice example:

#### Managing a Kubernetes cluster

At a glance Kubernetes only needs `kubectl` and some authentication credentials, but in reality you actually need a specific version of `kubectl` and `helm` for that cluster as well as `openssl`, `telepresence` and maybe `jq` or `jp`. Plus since this cluster is on Google you need `gcloud` and you need `cloud_sql-proxy` to work with the Postgresql database you have on Google. This means you probably want `psql` with a specific version and maybe `dbeaver` to manage the database. Ah and to test changes locally you might want `docker` as well. Now you either create a single virtual machine that is entirely responsible for this one cluster or you hope all these tools will be compatible with any other cluster you spin up and no other project you work on accidentally changes these tools. In an ideal world you would have none of these tools installed globally and only accessible the moment you `cd kubernetes-project-directory`. This is what I'm going to show you how to do.

### Start Small

Lets start with pulling in the above tools in a bash shell without specifying any versions:

```
nix-shell -p kubernetes-helm kubernetes docker google-cloud-sdk openssl postgresql100 jq jp
```

Note: if this takes too long, ctrl-c and remove a few. (unless you
actually have a cluster you want to try this with)

This gives you a bash shell with the tools these packages provide in your path and brings in anything you already have in your environment (like `vim`). Note, I removed `telepresence` because the version in the unstable NixPkgs set fails to build. Later on we will add it back in.

Now, lets turn this command into a file, create a file called `shell.nix` with these contents:

```
let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell { buildInputs = with pkgs; [ kubernetes-helm kubernetes
                                            docker google-cloud-sdk openssl postgresql100 jq jp]; }
```

Then call `nix-shell` in the directory with the `shell.nix`. It should come up immediately since with the previous command you already downloaded or built all the tools. See [this link](https://nixos.org/nixos/nix-pills/basics-of-language.html) for an introduction of the Nix language, just ignore the instruction to install `nix-repl` which is now bundled by default as `nix repl`. The magic is the "import <nixpkgs> {};"


### Approaching Reproducibility 

TODO

## dirty-go-dev-setup

Often, when I first started using NixOS, I would question: "How do I
do the normal things I'm used to in NixOS with Nix?". Think, you have
some project you need to build and don't have the time to figure out
the proper Nix way or how Nixpkgs does things for that language. In
reality what you are asking is "how do I use nix-shell to setup my
environment?". This challenge is not about reproducibility, but
dealing with real world dirty setups.


### Usability

Note: You will need to setup `direnv` if you want your shell to bring up the environment for you, [see direnv github page](https://github.com/direnv/direnv/wiki/Nix), instead of calling `nix-shell` manually.

Another file called `.envrc` with:

```
use_nix
```

