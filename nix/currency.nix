{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

let
    iso3166CountryCodes = (import <nixpkgs> {}).callPackage ./iso3166-country-codes.nix { inherit haskellPackages; };
in
haskellPackages.cabal.mkDerivation (self: with haskellPackages; {
  pname = "currency";
  version = "0.2.0.0";
  sha256 = "0yj1x7zmkmwr9az55i9gvf84m7i3b4qi80p8qk9hszzlv7rigmdw";
  buildDepends = [ hashable iso3166CountryCodes ];
  meta = {
    homepage = "https://github.com/singpolyma/currency-haskell";
    description = "Types representing standard and non-standard currencies";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
