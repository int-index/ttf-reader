{
  description = "ttf-reader";
  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      ghc = "ghc96";
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages =
        pkgs.haskell.packages.${ghc}.extend(hself: hsuper: {
          ttf-reader = haskellPackages.callCabal2nix "ttf-reader" "${self}" {};
        });
    in
    {
      packages.${system}.ttf-reader = haskellPackages.ttf-reader;
      defaultPackage.${system} = self.packages.${system}.ttf-reader;
      devShell.${system} = pkgs.mkShell {
        buildInputs = [
          haskellPackages.hie-bios
          haskellPackages.haskell-language-server
          haskellPackages.cabal-install
        ];
        inputsFrom = builtins.attrValues self.packages.${system};
      };
    };
}
