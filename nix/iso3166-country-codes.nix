{ cabal }:

cabal.mkDerivation (self: {
  pname = "iso3166-country-codes";
  version = "0.20130302.4";
  sha256 = "1mifah3bjhvja524ssj58f9hag6yx3lah8qlavvgvz9xlhlpxz19";
  meta = {
    description = "A datatype for ISO 3166 country codes";
    license = "LGPL";
    platforms = self.ghc.meta.platforms;
  };
})
