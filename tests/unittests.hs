import           Prelude

import           Tests.Matrix.Pipelined as Pipelined
import           Tests.Matrix.Simple    as Simple
import           Tests.Matrix.Sized     as Sized

main :: IO ()
main = do
  Simple.spec
  Sized.spec
  Pipelined.spec
