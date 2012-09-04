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

type Keyword = String
type Title   = String

getAndPrintHeadlines :: Conf -> IO ()
getAndPrintHeadlines conf = do
	ts <- mapM (getFeedTitles (proxy conf)) (urls conf)
	mapM_ printFeedTitles ts

countAndPrint :: Conf -> [Keyword] -> IO ()
countAndPrint conf keywords = do
	ts <- mapM (getFeedTitles (proxy conf)) (urls conf)
	let flattened_ts = prepareNewsData ts
	let stats        = keywordStats keywords flattened_ts
	if optVerbose (opts conf)
	  then mapM_ (putStrLn . statLayoutVerbose) stats
	  else mapM_ (putStrLn . statLayoutShort) stats

runTasks :: Conf -> IO ()
runTasks conf = do
	ts <- mapM (getFeedTitles (proxy conf)) (urls conf)
	let lfts = prepareNewsData ts
	mapM_ (runTask lfts) (tasks conf)

runTask :: [Title] -> Task -> IO ()
runTask titles t = do
	let (_, count, _) = singleKeywordStats titles (keyword t)
	if count >= threshold t
	  then do
	    putStrLn $ "threshold " ++ show (threshold t) ++
	      " reached for " ++ keyword t ++ " (" ++ show count ++ ")"
	    _ <- runCommand $ action t
	    return ()
	  else putStrLn $ "threshold " ++ show (threshold t) ++
	         " not reached for " ++ keyword t ++ " (" ++ show count ++ ")"

getFeedTitles :: Proxy -> String -> IO [String]
getFeedTitles prox url = do
	tags <- fmap parseTags $ getPage prox url
	let titles = partitions (~== "<title>") tags
	return $ map (fromTagText . (!! 1)) titles

printFeedTitles :: [String] -> IO ()
printFeedTitles = mapM_ putStrLn

getPage :: Proxy -> String -> IO String
getPage prox url = do
	(_,rsp) <- Network.Browser.browse $ do
	  setProxy prox
	  setOutHandler $ const (return ()) -- no debug output
	  request (getRequest url)
	return (rspBody rsp)

prepareNewsData :: [[String]] -> [String]
prepareNewsData = map lowerString . concat

lowerString :: String -> String
lowerString = map toLower

-- combine a stat tuple to a short string without the titles
statLayoutShort :: (Keyword, Int, [Title]) -> String
statLayoutShort (a, b, _) = a ++ "\t" ++ show b
	
-- combine a stat tuple to a verbose string
statLayoutVerbose :: (Keyword, Int, [Title]) -> String
statLayoutVerbose (a, b, c) =
    a ++ "\t" ++ show b ++ "\n" ++ flatten (appNewLine c)
	where
	  appNewLine :: [String] -> [String]
	  appNewLine = map (++ "\n") 
	  flatten :: [String] -> String
	  flatten = foldl (++) ""

-- stats for all given keywords
keywordStats :: [Keyword] -> [String] -> [(Keyword, Int, [Title])]
keywordStats kwords titles = map (singleKeywordStats titles) kwords
 
-- stats for a single keyword in title list, count appearances in a title only
-- once
singleKeywordStats :: [Title] -> Keyword -> (Keyword, Int, [Title])
singleKeywordStats titles kword =
    let hits  = filter (=~ kword) titles
        count = length hits
    in (kword, count, hits)

