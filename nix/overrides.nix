{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  contiguous-mmap = (
    with rec {
      contiguous-mmapSource = pkgs.lib.cleanSource ../.;
      contiguous-mmapBasic  = self.callCabal2nix "contiguous-mmap" contiguous-mmapSource { };
    };
    overrideCabal contiguous-mmapBasic (old: {
    })
  );
}
