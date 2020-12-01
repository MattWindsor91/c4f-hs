import Control.Monad (unless)
import System.Exit (exitFailure)
import System.IO (stderr, stdout, BufferMode (LineBuffering), hSetBuffering)
import Test.Fir.Atomic.MemOrder
import Test.Fir.Const
import Test.Fir.Id
import Test.Fir.Lvalue
import Test.Fir.OpAlgebra
import Test.Fir.Stmt.Stmt

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence
    [ Test.Fir.Atomic.MemOrder.tests
    , Test.Fir.Const.tests
    , Test.Fir.Id.tests
    , Test.Fir.Lvalue.tests
    , Test.Fir.OpAlgebra.tests
    , Test.Fir.Stmt.Stmt.tests
    ]

  unless (and results) exitFailure
