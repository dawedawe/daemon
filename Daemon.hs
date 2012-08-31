module Daemon
( getAndPrintHeadlines
, countAndPrint
, runTasks
) where

import Data.Char
import Network.HTTP
import Network.Browser
import System.Process (runCommand)
import Text.HTML.TagSoup
import Text.Regex.Posix ((=~))

import Conf

getAndPrintHeadlines :: Conf -> IO ()
getAndPrintHeadlines conf = do
	ts <- mapM (getFeedTitleStrings (proxy conf)) (urls conf)
	mapM_ printFeedTitleStrings ts

countAndPrint :: Conf -> [String] -> IO ()
countAndPrint conf keywords = do
	ts <- mapM (getFeedTitleStrings (proxy conf)) (urls conf)
	let flattened_ts = prepareNewsData ts
	let counts = countWordsInWordsVerb keywords flattened_ts
	if optVerbose (opts conf)
	  then mapM_ (putStrLn . combVerb) $ zip keywords counts
	  else mapM_ (putStrLn . comb) $ zip keywords (map fst counts)

runTasks :: Conf -> IO ()
runTasks conf = do
	ts <- mapM (getFeedTitleStrings (proxy conf)) (urls conf)
	let lfts = prepareNewsData ts
	mapM_ (runTask lfts) (tasks conf)

runTask :: [String] -> Task -> IO ()
runTask news t = do
	let count = countWordInWords (keyword t) news
	if count >= threshold t
	  then do
	    putStrLn $ "Threshold " ++ show (threshold t) ++
	      " exceeded for " ++ keyword t
	    _ <- runCommand $ action t
	    return ()
	  else putStrLn $ "Threshold " ++ show (threshold t) ++
	         " not exceeded for " ++ keyword t

getFeedTitleStrings :: Proxy -> String -> IO [String]
getFeedTitleStrings prox url = do
	tags <- fmap parseTags $ getPage prox url
	let titles = partitions (~== "<title>") tags
	return $ map (fromTagText . (!! 1)) titles

printFeedTitleStrings :: [String] -> IO ()
printFeedTitleStrings = mapM_ putStrLn

getPage :: Proxy -> String -> IO String
getPage prox url = do
	(_,rsp) <- Network.Browser.browse $ do
	  setProxy prox
	  setOutHandler $ const (return ()) -- no debug output
	  request (getRequest url)
	return (rspBody rsp)

prepareNewsData :: [[String]] -> [String]
prepareNewsData = map lowerString . concat

countWordsInWordsVerb :: [String] -> [String] -> [(Int, [String])]
countWordsInWordsVerb [] _      = []
countWordsInWordsVerb _ []      = []
countWordsInWordsVerb (x:xs) ws =
	countWordInWordsVerb x ws : countWordsInWordsVerb xs ws

countWordInWordsVerb :: String -> [String] -> (Int, [String])
countWordInWordsVerb "" _ = (0, [])
countWordInWordsVerb _ [] = (0, [])
countWordInWordsVerb w ws = helper (lowerString w) (0, []) ws
	where
	  helper :: String -> (Int, [String]) -> [String] -> (Int, [String])
	  helper _ _ [] = (0, [])
	  helper w' (i, ins) [x]
	    | x =~ w'   = (i + 1, x : ins)
	    | otherwise = (i, ins)
	  helper w' (i, ins) (x:xs)
	    | x =~ w'   = helper w' (i + 1, x : ins) xs
	    | otherwise = helper w' (i, ins) xs

countWordInWords :: String -> [String] -> Int
countWordInWords "" _      = 0
countWordInWords word news = sum $ map (=~ word) news

comb :: (String, Int) -> String
comb (a, b) = a ++ "\t" ++ show b
	
combVerb :: (String, (Int, [String])) -> String
combVerb (a, b) = a ++ "\t" ++ show (fst b) ++ "\n" ++ flat (appNewLine $ snd b)
	where
	appNewLine :: [String] -> [String]
	appNewLine = map (++ "\n") 
	flat :: [String] -> String
	flat = foldl (++) ""

lowerString :: String -> String
lowerString = map toLower

