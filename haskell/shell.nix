with import <nixpkgs> {};
let 
  ps1903 = import ./nixpkgs.nix;
  myghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    base
    time
    aeson
    http-client
    sort
    mtl
    utf8-string
    wai-cors
    scotty
  ]);

in
stdenv.mkDerivation rec {
  name = "elm-env";
  buildInputs = [
    figlet
    ps1903.git
    myghc
  ];
  shellHook = ''
    #figlet "Welcome to the Checkout-Order-Total Kata"
    ghci
  '';
}