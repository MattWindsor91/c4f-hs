import Control.Monad (unless)
import System.Exit (exitFailure)
import System.IO (stderr, stdout, BufferMode (LineBuffering), hSetBuffering)
import Test.Fir.Id
import Test.Fir.Lvalue

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence
    [ Test.Fir.Id.tests
    , Test.Fir.Lvalue.tests
    ]

  unless (and results) exitFailure
