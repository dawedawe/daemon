module Daemon where

import Data.Char
import qualified Data.Text as T
import Network.HTTP
import Network.Browser
import Text.HTML.TagSoup

getUrls :: FilePath -> IO [String]
getUrls path = do
	s <- readFile path
	return $ lines s

getFeedTitleStrings :: String -> IO [String]
getFeedTitleStrings url = do
	tags <- fmap parseTags $ getPage' (Proxy "localhost:8118" Nothing) url
	let titles = partitions (~== "<title>") tags
	return $ map (fromTagText . (!! 1)) titles

printFeedTitleStrings :: [String] -> IO ()
printFeedTitleStrings s = mapM_ putStrLn s

countAndPrint :: [String] -> [String] -> IO ()
countAndPrint urls keywords = do
	ts <- mapM getFeedTitleStrings urls
	let flattened_ts = flatten ts
	let counts = countWordsInWords keywords flattened_ts
	mapM_ (putStrLn . comb) $ zip keywords counts

getAndPrintHeadlines :: [String] -> IO ()
getAndPrintHeadlines urls = do
	ts <- mapM getFeedTitleStrings urls
	mapM_ printFeedTitleStrings ts

getPage :: String -> IO String
getPage url = simpleHTTP (getRequest url) >>= getResponseBody

getPage' :: Proxy -> String -> IO String
getPage' proxy url =
	do
		(_,rsp) <- Network.Browser.browse $ do
			setProxy proxy
			setOutHandler $ const (return ())
			request (getRequest url)
		return (rspBody rsp)

countWordInWords :: String -> [String] -> Int
countWordInWords "" _ = 0
countWordInWords _ [] = 0
countWordInWords w ws = helper (T.pack w) 0 (map T.pack ws)
	where
		helper :: T.Text -> Int -> [T.Text] -> Int
		helper _ _ []				= 0
		helper w' i [(x)]
			| (T.isInfixOf w' x)	= (i+1)
			| otherwise				= i
		helper w' i (x:xs)
			| (T.isInfixOf w' x)	= helper w' (i+1) xs
			| otherwise				= helper w' i xs

countWordsInWords :: [String] -> [String] -> [Int]
countWordsInWords swords wrds = countWordsInWords' lswords lwords
	where
		lswords = (map lowerString swords)
		lwords  = (map lowerString wrds)
		countWordsInWords' :: [String] -> [String] -> [Int]
		countWordsInWords' [] _ = []
		countWordsInWords' _ [] = []
		countWordsInWords' (x:xs) ws =
			(countWordInWords x ws) : (countWordsInWords' xs ws)

comb :: (String, Int) -> String
comb (a, b) = a ++ "\t" ++ (show b)
	
flatten :: [[a]] -> [a]
flatten = foldl (++) []

lowerString :: String -> String
lowerString s = map toLower s
