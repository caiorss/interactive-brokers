{ haskellPackages ? (import <nixpkgs> {}).haskellPackages
}:

let 
    pkgs = (import <nixpkgs> {});
    currency = (import <nixpkgs> {}).callPackage ./nix/currency.nix { inherit haskellPackages; };
in 
haskellPackages.cabal.mkDerivation (self: with haskellPackages; with pkgs; {
  pname = "interactive-brokers";
  version = "0.1.0.0";
  src = ./.;
  buildTools = [ cabalInstall_1_20_0_3 ];
  buildDepends = [
    attoparsec 
    dataDefault
    mtl 
    time 
    transformers 
    currency
    lens
    network
    networkSimple
    split
    time
    tz
  ];

  doCheck = false;
  meta = {
    homepage = "www.github.com/RobinKrom/interactive-brokers";
    description = "Library for Interactive Brokers api.";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
