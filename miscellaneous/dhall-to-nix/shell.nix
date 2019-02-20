let
  tarball-1809 = fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz;
    sha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
  };


  pkgs = import tarball-1809 { };


  inherit (pkgs) dhallToNix;

in
  { example0 = dhallToNix "λ(x : Bool) → x == False" false;
    example1 = (dhallToNix "{ foo = 1, bar = True }").foo;
    example2 = dhallToNix "List/reverse Integer" [1 2 3];
  }
