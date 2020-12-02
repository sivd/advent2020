import Control.Concurrent
import Data.Char
import Data.List.Split
import Debug.Trace


data Puzzle = MkPuzzle {lower :: Int, upper :: Int, char :: Char, password :: String}
  deriving Show

convert :: String -> Puzzle
convert string = MkPuzzle (read lw :: Int) (read up :: Int) (head chr) (head pword)
  where
    (lw:up:chr:_:pword) = splitOneOf "- :" string

-- part 1
calculate :: [Puzzle] -> IO ()
calculate list = do 
  result <- newMVar (0 :: Int)
  mapM_ (func' result) list
  readMVar result >>= print
  where
    func :: Puzzle -> Bool
    func p = len p >= lower p && len p <= upper p
    func' :: MVar Int -> Puzzle -> IO ()
    func' mvar p  | func p = do
                    old <- takeMVar mvar
                    putMVar mvar (old + 1)
                  | otherwise = return ()
    len p = length [x | x <- password p, x == char p] -- how many of that character is in the password


-- part 2
calculate' :: [Puzzle] -> IO ()
calculate' list = do
  result <- newMVar (0 :: Int)
  mapM_ (func' result) list
  readMVar result >>= print
  where
    -- 1-based, so -1 on the index
    func :: Puzzle -> Bool
    func p  |    password p !! (lower p - 1) == char p && password p !! (upper p - 1) /= char p
              || password p !! (lower p - 1) /= char p && password p !! (upper p - 1) == char p
              = True
            | otherwise = False
    func' :: MVar Int -> Puzzle -> IO ()
    func' mvar p  | func p = do
                    old <- takeMVar mvar
                    putMVar mvar (old + 1)
                  | otherwise = return ()



main :: IO ()
main = do
  input <- readFile "input2.txt"
  let inputList = lines input
  let puzzleInput = map convert inputList
  --calculate puzzleInput
  calculate' puzzleInput