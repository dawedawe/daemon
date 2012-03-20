module Daemon
( getAndPrintHeadlines
, countAndPrint
, runTasks
) where

import Data.Char
import qualified Data.Text as T
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
	let counts = countWordsInWords keywords flattened_ts
	if optVerbose (opts conf) == True
	  then mapM_ (putStrLn . comb') $ zip keywords counts
	  else mapM_ (putStrLn . comb) $ zip keywords (map fst counts)

runTasks :: Conf -> IO ()
runTasks conf = do
	ts <- mapM (getFeedTitleStrings (proxy conf)) (urls conf)
	let lfts = prepareNewsData ts
	mapM_ (runTask lfts) (tasks conf)

runTask :: [String] -> Task -> IO ()
runTask news t = do
	let count = countWordInWords (keyword t) news
	if count >= (threshold t)
	  then do
	    putStrLn ("Threshold " ++ (show (threshold t)) ++
	      " exceeded for " ++ (keyword t))
	    _ <- runCommand $ action t
	    return ()
	  else putStrLn ("Threshold " ++ (show (threshold t)) ++
	         " not exceeded for " ++ (keyword t))

getFeedTitleStrings :: Proxy -> String -> IO [String]
getFeedTitleStrings prox url = do
	tags <- fmap parseTags $ getPage prox url
	let titles = partitions (~== "<title>") tags
	return $ map (fromTagText . (!! 1)) titles

printFeedTitleStrings :: [String] -> IO ()
printFeedTitleStrings s = mapM_ putStrLn s

getPage :: Proxy -> String -> IO String
getPage prox url = do
	(_,rsp) <- Network.Browser.browse $ do
	  setProxy prox
	  setOutHandler $ const (return ()) -- no debug output
	  request (getRequest url)
	return (rspBody rsp)

prepareNewsData :: [[String]] -> [String]
prepareNewsData news = map lowerString $ flatten news

countWordsInWords :: [String] -> [String] -> [(Int, [T.Text])]
countWordsInWords [] _ = []
countWordsInWords _ [] = []
countWordsInWords (x:xs) ws =
	(countWordInWords' x ws) : (countWordsInWords xs ws)

countWordInWords' :: String -> [String] -> (Int, [T.Text])
countWordInWords' "" _ = (0, [])
countWordInWords' _ [] = (0, [])
countWordInWords' w ws = helper (T.pack (lowerString w)) (0, []) (map T.pack ws)
	where
	  helper :: T.Text -> (Int, [T.Text]) -> [T.Text] -> (Int, [T.Text])
	  helper _ _ []          = (0, [])
	  helper w' (i, ins) [(x)]
	    | (T.isInfixOf w' x) = ((i+1), (x : ins))
	    | otherwise		 = (i, ins)
	  helper w' (i, ins) (x:xs)
	    | (T.isInfixOf w' x) = helper w' ((i+1), (x : ins)) xs
	    | otherwise          = helper w' (i, ins) xs

countWordInWords :: String -> [String] -> Int
countWordInWords "" _ = 0
countWordInWords word news = sum $ map (flip (=~) word) news

comb :: (String, Int) -> String
comb (a, b) = a ++ "\t" ++ (show b)
	
comb' :: (String, (Int, [T.Text])) -> String
comb' (a, b) = a ++ "\t" ++ (show $ fst b) ++ "\n" ++ (show $ snd b)

flatten :: [[a]] -> [a]
flatten = foldl (++) []

lowerString :: String -> String
lowerString = map toLower

