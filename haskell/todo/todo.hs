{-# OPTIONS_GHC -Wall -Werror #-}

{-
 - A simple todo list manipulator to do the following things:
 - - View tasks
 - - Add tasks
 - - Delete tasks
 - - Bump priority of the task
 -
 - Majority of the code from http://learnyouahaskell.com/input-and-output#command-line-arguments
-}

import System.Environment          -- getArgs
import System.Directory            -- removeFile
import System.IO                   -- openFile, openTempFile, getContents, hGetContents, putStr
import qualified Data.List as List -- splitAt, delete

-- List from command line argument to the function that is supposed to be called for that argument
dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           , ("bump", bump)
           ]

main :: IO ()
main = do
   (command:args) <- getArgs
   let (Just action) = lookup command dispatch -- TODO (inception) fix when this fails
   action args

-- Takes a fileName and a todoItem, either creates or appends that time to file
add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = error "Not possible!"

-- Take a fileName and enumerates todo tasks
view :: [String] -> IO ()
view [fileName] = do
   contents <- readFile fileName
   let tasks = zipWith (\n task -> show n ++ " - " ++ task) [(0 :: Int)..] (lines contents)
   putStr $ unlines tasks
view _ = error "Not possible!"

-- Takes a fileName and an index (obtained from the view) and deletes the task
remove :: [String] -> IO ()
remove inp = twoFileImpl inp deleteByIndex

-- Takes a fileName and an index (obtained from the view) and puts that task on top of the list
bump :: [String] -> IO ()
bump inp = twoFileImpl inp toHead

-- Takes a num (0 indexed) and a list, and moves the list !! num to the head
toHead :: Int -> [a] -> [a]
toHead n xs
   | n >= length xs = []
   | otherwise = [x] ++ pre ++ post
      where (pre, x:post) = List.splitAt n xs

-- Takes a num (0 indexed) and a list, and deletes list !! num
deleteByIndex :: (Eq a) => Int -> [a] -> [a]
deleteByIndex n xs
   | n >= length xs = [] -- ideally we should raise an exception
   | otherwise = List.delete (xs !! n) xs

-- Both delete and bump do similar things
-- open a file, read it, get hold of an index, apply some func (delete or bump) on that file
-- and write to a separate file. So factoring out the common code
twoFileImpl :: [String] -> (Int -> [String] -> [String]) -> IO ()
twoFileImpl [fileName, taskNo] f = do
   handle <- openFile fileName ReadMode
   (tempName, tempHandle) <- openTempFile "." "temp"
   contents <- hGetContents handle
   let num = read taskNo
       tasks = lines contents
       newToDos = f num tasks
   hPutStr tempHandle $ unlines newToDos
   hClose tempHandle
   hClose handle
   removeFile fileName
   renameFile tempName fileName
twoFileImpl _ _ = error "Not possible!"
