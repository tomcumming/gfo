{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" ];
    in
    builtins.foldl'
      (ss: s:
        let
          pkgs = nixpkgs.legacyPackages."${s}";
        in
        ss //
        {
          devShells."${s}".default = pkgs.mkShell {
            packages = [
              pkgs.cabal-install
              pkgs.ghc
              pkgs.haskellPackages.haskell-language-server
              pkgs.ormolu
            ];
          };
          formatter."${s}" = pkgs.nixpkgs-fmt;
        })
      { }
      systems;
}
