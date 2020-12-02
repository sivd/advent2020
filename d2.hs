import Control.Concurrent
import Data.Char
import Data.List.Split

-- part 1

data Puzzle = MkPuzzle {lower :: Int, upper :: Int, char :: Char, password :: String}
  deriving Show

convert :: String -> Puzzle
convert string = MkPuzzle (read lw :: Int) (read up :: Int) (head chr) (head pword)
  where
    (lw:up:chr:_:pword) = splitOneOf "- :" string

calculate :: [Puzzle] -> IO ()
calculate list = do 
  result <- newMVar (0 :: Int)
  mapM_ (func' result) list
  readMVar result >>= print
    where
      func :: Puzzle -> Bool
      func p = len p >= lower p && len p <= upper p
      func' :: MVar Int -> Puzzle -> IO ()
      func' mvar p = case func p of 
        True -> do
          old <- takeMVar mvar
          putMVar mvar (old + 1)
        False -> return ()
      len p = length [x | x <- password p, x == char p] -- how many of that character is in the password


main :: IO ()
main = do
  input <- readFile "input2.txt"
  let inputList = lines input
  let puzzleInput = map convert inputList
  calculate puzzleInput