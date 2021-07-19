import Prelude

import Test.Tasty

import qualified Tests.Example.Project as Proj
import Tests.Example.Simple.Matrix as Simple
import Tests.Example.Sized.Matrix as Sized

main :: IO ()
main = do
  Simple.spec
  Sized.spec
  Proj.main
