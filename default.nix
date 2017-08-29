with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "copper";
  ghc = haskellPackages.ghc;
  buildInputs = [ git # To enable git packages in stack.yaml
                  cabal-install
                  zlib ]; # For stack solver

}
