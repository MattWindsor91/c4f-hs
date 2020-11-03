import Control.Monad (unless)
import System.Exit (exitFailure)
import System.IO (stderr, stdout, BufferMode (LineBuffering), hSetBuffering)
import Test.Fir.Const
import Test.Fir.Id
import Test.Fir.Lvalue
import Test.Fir.OpAlgebra

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence
    [ Test.Fir.Const.tests
    , Test.Fir.Id.tests
    , Test.Fir.Lvalue.tests
    , Test.Fir.OpAlgebra.tests
    ]

  unless (and results) exitFailure
