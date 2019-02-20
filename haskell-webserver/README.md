# Description

This set of challenges are designed to help you learn how to build and
deploy code using Nix. Its broken down into three main sections: build
the Haskell webserver, deploy the Haskell webserver on a VM (like
digital ocean) and create a docker image with the Haskell webserver.

I highly recommend you don't develop off of unstable (at least for
Haskell) so you can have fast build times. Regardless you will want to
set the package set anyways for reproducibility. Add this to your
derivations:

```
tarball-1809 = fetchTarball {
  url = https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz;
  sha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
};

pkgs = import tarball-1809 { };
```

Note: you don't have to do them in order. Except, the Docker one
depends on the "deploy-to-vm" challenge.

Note2: The code is from this tutorial: 
https://www.schoolofhaskell.com/school/advanced-haskell/building-a-file-hosting-service-in-yesod/part%204

## Building Projects in Haskell

Note: when starting a new project you usually start with a nix-shell
that has the basics to get started: `nix-shell -p cabal-install hpack`
Since I've provided the code, we are going to go right into what a
setup repository will look like.

There are two files you will want to have `shell.nix` and
`default.nix`. The first is what defines your development environment
and the later defines how to build your tool. That way you can call
`nix-shell` (or direnv will do it for you) to get all the tools you
need and `nix-build` to build and put the completed executable in `./result`.

Fortunately for us, there is something called `callCabal2nix` that can
both give us a derivation to build our tool and a development
environment. Your task is to figure out how to use `callCabal2nix` for
both and how to use `overrideAttrs` to add extra development tools to
your development environment like `hpack`, `dash-haskell` and `ghcid`.

After you get your `shell.nix` setup use `dash-haskell` to build docs
for the code `dash-haskell -c webserver.cabal` which you can open with
either `zeal` (Linux) or `dash` (MacOS). You can even make `zeal` one
of the tools your `shell.nix` brings in.

Note: the other reason you often want to have a separate `default.nix`
is so Hydra can build your tool (if you ever go down that route).

Note2: you can run the result with `./result/bin/webserver` (the
Haskell-Nix tooling puts the executable inside of `bin`

Note3: you will want to use `ghc822` and its package set.

### Adding dash-haskell

If you try to add `dash-haskell` to your `buildInputs` it will fail to
build because the latest build of `dash-haskell` on Nixpkgs is broken.
Now, we still want to use it to create our docsets, but we don't want
to try using a whole new package set just for this one tool. This,
mini, challenge is about finding the right revision and adding it to
your `shell.nix`.

You will want to lookup `fetchFromGitHub`. Also, for the sha256, just
put in something random of the correct length and then from the error
you can populate the correct sha256. Yes, this is not a secure or nice
way to get the sha256 (I welcome any suggestions on how to do this
properly).

Note: This is a long rabbit hole that will show you the ugly side of
getting poorly maintained projects to build.

## Lets deploy It!

Now that you have gotten a development environment and can build the
code successfully its time to deploy your code online! 

Ok, first go and create a VM and install Nix on it (the same way you
install Nix on your machine). Don't worry I'll still be here when you
get back.... Oh, and its easier if you crate an entry in your
`.ssh/config` with the login details.

Normally you could just deploy your `./result`, but the closure (ie.
things needed to build your tool) contains GHC and documentation. You
do not want to deploy all of that. At the same time you don't want to
change your development environment (`default.nix`) where you want
those things. So, you are going to create a `release.nix` who's
derivation will result in just the executable. Hint, lookup
`justStaticExecutables`. The result can be very similar to your
`default.nix` depending on if you are following Gabriel's tutorial or
not.

(insert time you spend working and asking questions)

Now for some magic run:

```
nix-copy-closure --to <your entry in .ssh/config> ./result
ssh <your entry in .ssh/config>
nix-env -i /nix/store/<long path nix-copy-closure output at the end>
```

The last one puts your `webserver` executable on your path

Now you will have the executable with all of its runtime dependencies
(no more pain using CentOS). Run and enjoy! (you might need to open
ports)

Note: if you want extra credit adjust your `release.nix` to pull a
specific commit of the code (this assumes you push it to your
github/gitlab) making it independent of your codebase. Now you have a
file describing how to reproducible build your code at that point in
time.

## Build Docker Image (for things like Kuberenets)

Now, you have learned quite a bit about using Nix for real. You will
want to look at Gabriel's tutorial (project3) to get the gist of how
to do this. However, you can also use the methods you have already
been learning to lookup `dockerTools.buildImage` and try it yourself.

Note: your docker image must have a `/tmp` or your webserver will
crash. I'm not sure if its the Haskell runtime or one of the
dependencies that needs it.

### Resources

My solution is based on project3 from this tutorial:
https://github.com/Gabriel439/haskell-nix

### How to Look things up

There are several ways to lookup what things are: 

#### First Option

You can use: https://search.nix.gsc.io/ to do a regex search
over all of Nixpkgs, however, you don't get to chose which revision of
Nixpkgs (I think it defaults to unstable).

#### Second Option

You can get a copy of the Nixpkgs revision you are using by
putting the below in a file `test.nix`:

```
let
  tarball-1809 = fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz;
    sha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
  };

in
  { inherit tarball-1809; }

```

Then calling
```
nix-instantiate --read-write-mode --eval --strict test.nix
```
will output: 
```
{ tarball-1809 = "/nix/store/iam5nc2mbh7nssw5xji5q54gbnnbvjwb-source"; }
```
Then you can:
```
cd /nix/store/iam5nc2mbh7nssw5xji5q54gbnnbvjwb-source
ag <text to search for>
grep <text to search for>
```

#### Third Option

If you want to use auto completion to discover based on the
hierarchy what `pkgs` actually contains create another `test.nix` with
this inside:

```
let
  tarball-1809 = fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz;
    sha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
  };

  pkgs = import tarball-1809 { };
in
 { inherit pkgs; }
```

Then call `nix repl test.nix` to get a nix interpreter with everything
you have available to build your derivations. Type `pkgs.haskell.lib.<tab>` 
to get a list of the names of provided Haskell related build
functions and so on. Sadly, the repl cannot tell you what their
arguments are or what they do.

#### Fourth Option

Just use the function the way you think it works and then from the
errors Nix gives you can lookup how to use the function. (Yes its
hacky). However, keep in mind, Nix is lazy. If you just call nix-shell
or nix-build it may not evaluate your incorrect function call and then
you won't get any errors. 

Note: I picked the 18.09 Nixpkgs release because, at the time of
writing, it was the latest, so most things you can download from the
Nixpkgs build servers. The trick is to use:

```
nix-instantiate --read-write-mode --eval --strict test.nix
```

For Example, put this in test.nix to "lookup" `buildImage`:

```
let
  tarball-1809 = fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz;
    sha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
  };

  check = pkgs.dockerTools.buildImage {};

  pkgs = import tarball-1809 { };
in
 { inherit check; }
```

Then you will get back:

```
error: 'buildImage' at /nix/store/iam5nc2mbh7nssw5xji5q54gbnnbvjwb-source/pkgs/build-support/docker/default.nix:420:16 called without required argument 'name', at /home/kusanagi/Projects/Non-Work/nix-tutorial/haskell-webserver/test.nix:7:11
(use '--show-trace' to show detailed location information)
```

Then you can:

```
vim /nix/store/iam5nc2mbh7nssw5xji5q54gbnnbvjwb-source/pkgs/build-support/docker/default.nix
(then go to line 420)
```

Note: You have to give something of the correct type to functions in
order for Nix to give you an error where the function was created.
Most functions take a set and usually have a required argument, so an
empty set `{}` tends to do the trick.

Note2: don't evaluate strict the whole `pkgs`. In other words, don't
have `{ inherit pkgs; }` at the end there. Fortunately it dies on
an unrelated error instead of trying to evaluate every single package
in nixpkgs...

# Notes

If you want to get `direnv` setup, requires:
https://nixos.wiki/wiki/Development_environment_with_nix-shell
https://github.com/direnv/direnv/wiki/Nix

- `echo "use_nix" > .envrc`
