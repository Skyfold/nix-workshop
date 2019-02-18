# Description

This is some simple go-lang code to demo WebSockets.

# Goal

Write a single `shell.nix` where calling `nix-shell` lets you run:
	- `go get` for dependencies
	- run demo with `go run main.go`

All you need to do is the `go` executable and set your `GOPATH` to `$(pwd)`.

# Links

https://nixos.org/nixos/nix-pills/developing-with-nix-shell.html

Note: `shellHook` attribute will be useful, search for it in: https://nixos.org/nix/manual/

# Notes

To get the dependencies run:
  - go get
 
Note: you may get this error (depending on how you wrote the shell.nix):

`go get: no install location for directory /home/kusanagi/Projects/Non-Work/nix-tutorial/miscellaneous/dirty-go-dev-setup outside GOPATH
	For more details see: 'go help gopath'`

Its because I didn't want to put the single main.go file inside of a gopath.

To run the code:
  - go run main.go
