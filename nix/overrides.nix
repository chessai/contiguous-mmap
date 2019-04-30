{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  contiguous = self.callCabal2nix "contiguous" (pkgs.fetchFromGitHub {
    owner = "andrewthad";
    repo = "contiguous";
    rev = "5ad00154f59349288b037f94cb250bfdd0516a36"; 
    sha256 = "1z1hspiy1l0lhm6n7dznyxmjawqbb8z4x4ip28xm4gm3dk7dsn60";
  }) {};

  contiguous-mmap = (
    with rec {
      contiguous-mmapSource = pkgs.lib.cleanSource ../.;
      contiguous-mmapBasic  = self.callCabal2nix "contiguous-mmap" contiguous-mmapSource { };
    };
    overrideCabal contiguous-mmapBasic (old: {
    })
  );
}
