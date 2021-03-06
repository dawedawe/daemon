module Daemon
( getAndPrintHeadlines
, countAndPrint
, runTasks
) where

import Control.Monad (when)
import Data.Char
import Network.HTTP
import Network.Browser
import System.Process (runCommand)
import Text.HTML.TagSoup
import Text.Regex.Posix ((=~))

import Conf

type Keyword = String
type Title   = String

-- |Get the feed titles and print them.
getAndPrintHeadlines :: Conf -> IO ()
getAndPrintHeadlines conf = do
	ts <- mapM (getFeedTitles (proxy conf)) (urls conf)
	mapM_ printFeedTitles ts

-- |Get the feed titles, count the keyword appearances and print the result.
countAndPrint :: Conf -> [Keyword] -> IO ()
countAndPrint conf keywords = do
	ts <- mapM (getFeedTitles (proxy conf)) (urls conf)
	let flattened_ts = prepareNewsData ts
	let stats        = keywordStats keywords flattened_ts
	if optVerbose (opts conf)
	  then mapM_ (putStrLn . statLayoutVerbose) stats
	  else mapM_ (putStrLn . statLayoutShort) stats

-- |Get the feed titles and process the tasks configuration.
runTasks :: Conf -> IO ()
runTasks conf = do
	ts <- mapM (getFeedTitles (proxy conf)) (urls conf)
	let lfts = prepareNewsData ts
	mapM_ (runTask lfts) (tasks conf)

-- |Run a Task if it's threshold is reached and print a message to stdout.
runTask :: [Title] -> Task -> IO ()
runTask titles t = do
    let (_, count, _) = singleKeywordStats titles (keyword t)
    putStrLn (thresholdDispString t count)
    when (count >= threshold t) $
      do _ <- runCommand (action t)
         return ()

-- |Build display string for a task according to it's threshold and the count.
thresholdDispString :: Task -> Int -> String
thresholdDispString (Task kw th _) cnt =
    let n = if cnt < th then " not" else ""
    in  "threshold " ++ show th ++ n ++ " reached for " ++ kw ++
          " (" ++ show cnt ++ ")"

-- |Parse the titles out of the feed page, using a proxy if set.
getFeedTitles :: Proxy -> String -> IO [String]
getFeedTitles prox url = do
	tags <- fmap parseTags $ getPage prox url
	let titles = partitions (~== "<title>") tags
	return $ map (fromTagText . (!! 1)) titles

-- |Get the page using a proxy if set.
getPage :: Proxy -> String -> IO String
getPage prox url = do
	(_,rsp) <- Network.Browser.browse $ do
	  setProxy prox
	  setOutHandler $ const (return ()) -- no debug output
	  request (getRequest url)
	return (rspBody rsp)

-- |Print all titles to stdout.
printFeedTitles :: [String] -> IO ()
printFeedTitles = mapM_ putStrLn

-- |Concat and lowerString everything.
prepareNewsData :: [[String]] -> [String]
prepareNewsData = map lowerString . concat

-- |Transform a whole String to lower case.
lowerString :: String -> String
lowerString = map toLower

-- |Combine a stat tuple to a short string without the titles.
statLayoutShort :: (Keyword, Int, [Title]) -> String
statLayoutShort (a, b, _) = a ++ "\t" ++ show b
	
-- |Combine a stat tuple to a verbose string.
statLayoutVerbose :: (Keyword, Int, [Title]) -> String
statLayoutVerbose (a, b, c) =
    a ++ "\t" ++ show b ++ "\n" ++ flatten (appNewLine c)
	where
	  appNewLine :: [String] -> [String]
	  appNewLine = map (++ "\n") 
	  flatten :: [String] -> String
	  flatten = foldl (++) ""

-- |Stats for all given keywords.
keywordStats :: [Keyword] -> [String] -> [(Keyword, Int, [Title])]
keywordStats kwords titles = map (singleKeywordStats titles) kwords
 
-- |Stats for a single keyword in title list, count appearances in a title only
-- once.
singleKeywordStats :: [Title] -> Keyword -> (Keyword, Int, [Title])
singleKeywordStats titles kword =
    let
      hits  = filter (=~ kword) titles
      count = length hits
    in (kword, count, hits)

