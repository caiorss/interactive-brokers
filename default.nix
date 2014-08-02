{ haskellPackages ? (import <nixpkgs> {}).haskellPackages
}:


let 
    iso3166-country-codes = (import <nixpkgs> {}).callPackage ./nix/iso3166-country-codes.nix { cabal = haskellPackages.cabal; };
    network-simple = (import <nixpkgs> {}).callPackage ./nix/network-simple.nix { };
    old-locale = (import <nixpkgs> {}).callPackage ./nix/old-locale.nix { cabal = haskellPackages.cabal; }; 
    currency = (import <nixpkgs> {}).callPackage ./nix/currency.nix { cabal = haskellPackages.cabal; hashable = haskellPackages.hashable; inherit iso3166-country-codes;};
in 
haskellPackages.cabal.mkDerivation (self: with haskellPackages; {
  pname = "interactive-brokers";
  version = "0.1.0.0";
  src = ./.;
  buildTools = [ cabalInstall_1_20_0_3 ];
  buildDepends = [
    attoparsec 
    mtl 
    network
    time 
    transformers 
    currency
    lens
    network-simple
    old-locale
    split
    time
    tz
  ];

  doCheck = false;
  noHaddock = true;
  strictConfigurePhase = false;
  meta = {
    homepage = "www.github.com/RobinKrom/interactive-brokers";
    description = "Library for Interactive Brokers api.";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
