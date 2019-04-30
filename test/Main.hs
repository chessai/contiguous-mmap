module Main (main) where

import Data.Primitive.Array (Array,MutableArray)
import System.IO.Temp
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.Primitive.Array as Array
import qualified Data.Primitive.Contiguous as C
import qualified Data.Primitive.Contiguous.MMap as CM

main :: IO ()
main = quickCheck prop_read_write

prop_read_write :: [Double] -> Property
prop_read_write l = monadicIO $ do
  let arr = C.fromList l :: Array Double
  arr' <- run $ withSystemTempFile "contiguous-mmap-" $ \fn _ -> do
    _ <- CM.writeMMap fn arr
    CM.unsafeMMap fn CM.EntireFile
  assert (arr == arr')
