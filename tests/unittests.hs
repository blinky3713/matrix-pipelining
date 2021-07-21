import Prelude

import Test.Tasty

import qualified Tests.Example.Project as Proj
import Tests.Example.Simple.Matrix as Simple
import Tests.Example.Sized.Matrix as Sized
import Tests.Example.Pipelined.Matrix as Pipelined

main :: IO ()
main = do
  Simple.spec
  Sized.spec
  Pipelined.spec
  Proj.main
