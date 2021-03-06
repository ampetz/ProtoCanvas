module ParseFile where

import System.IO
import Data.Char
import Data.List.Split

--type Name = String
data MessageD = MessageD
                { from    :: String
                , to      :: String
                , message :: String
                } deriving (Show)


commaParse :: String -> [Int]
commaParse s = let xs' = splitOneOf "," s in
  map read xs'

fromFile :: FilePath -> IO [MessageD]
fromFile fp = do
  fileLines <- readFileLines fp
  --putStrLn $ show $ length fileLines
  mds <- parseLines fileLines
  return mds

parseLine :: String -> MessageD
parseLine s = let from = takeWhile (not . comma) s
                  rest = drop ((length from) + 1) s
                  rest' = dropWhile (isSpace) rest
                  to = takeWhile (not . colon) rest'
                  rest'' = drop ((length to) + 1) rest'
                  message = dropWhile (isSpace) rest'' in
              MessageD from to message
                 

 where comma :: Char -> Bool
       comma x = x == ','
       colon :: Char -> Bool
       colon x = x == ':'


parseLines :: [String] -> IO [MessageD]
parseLines xs = do
  let res = map parseLine xs
  --putStrLn $ show $ length res
  return res

 where f :: String -> MessageD
       f s = let from = takeWhile (not . comma) s
                 rest = drop ((length from) + 1) s
                 rest' = dropWhile (isSpace) rest
                 to = takeWhile (not . colon) rest'
                 rest'' = drop ((length to) + 1) rest'
                 message = dropWhile (isSpace) rest'' in
             MessageD from to message
                 

       comma :: Char -> Bool
       comma x = x == ','
       colon :: Char -> Bool
       colon x = x == ':'




       
readFileLines :: FilePath -> IO [String]
readFileLines = fmap lines . readFile


