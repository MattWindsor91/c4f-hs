import Control.Monad (unless)
import System.Exit (exitFailure)
import System.IO (stderr, stdout, BufferMode (LineBuffering), hSetBuffering)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [
    ]

  unless (and results) exitFailure
