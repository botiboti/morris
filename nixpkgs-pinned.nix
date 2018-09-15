# Define a pinned nixpkgs version
{ pkgs ? import <nixpkgs> {}, args ? {}, ...}:
let
  pinned_pkgs_path = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a25dcb66077310067ca1b59b2f38046bee752b0a";
    sha256 = "1qwlz1qbq9gi11z60n3bpj27gx61vdaf76akhmgsdl34s53nlnbf";
  };
in
  import pinned_pkgs_path args
