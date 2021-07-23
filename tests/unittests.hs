import           Prelude

import           Test.Tasty

import           Tests.Example.Pipelined.Matrix as Pipelined
import qualified Tests.Example.Project          as Proj
import           Tests.Example.Simple.Matrix    as Simple
import           Tests.Example.Sized.Matrix     as Sized

main :: IO ()
main = do
  Simple.spec
  Sized.spec
  Pipelined.spec
  Proj.main
