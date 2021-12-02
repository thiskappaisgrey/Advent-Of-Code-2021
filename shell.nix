{ pkgs ? import <nixpkgs> {} }:

let  ghc = pkgs.haskellPackages.ghcWithPackages (p: [ p.split p.text p.random ]); in
with pkgs;

mkShell {
  buildInputs = [
    ghc
    pkgs.haskell-language-server
  ];
}
