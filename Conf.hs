module Conf
( Conf (..)
, buildConf
) where

import qualified Data.ConfigFile as CF
import Data.Either.Utils (forceEither)
import Data.Maybe (fromJust)
import Network.Browser

data Conf = Conf {
	verbose	:: Bool,
	proxy :: Proxy,
	urls :: [String],
	tasks :: [Task]
}

data Task = Task {
	keyword :: String,
	threshold :: Int,
	action :: String
}

buildConf :: FilePath -> FilePath -> IO Conf
buildConf confpath feedpath = do
	items		<- getConfItems confpath
	feedurls	<- getUrls feedpath
	let prx		= getProxyConf items
	let ts		= getTasks items
	return $ Conf True prx feedurls ts

getConfItems :: FilePath -> IO [(CF.OptionSpec, String)]
getConfItems path = do
	val		<- CF.readfile CF.emptyCP path
	let cp	= forceEither val
	return $ forceEither $ CF.items cp "DEFAULT"

getProxyConf :: [(CF.OptionSpec, String)] -> Proxy
getProxyConf items = do
	mayToProxy $ lookup "proxy" items
	where
		mayToProxy :: Maybe String -> Proxy
		mayToProxy Nothing = NoProxy
		mayToProxy (Just s) = Proxy s Nothing

getTasks :: [(CF.OptionSpec, String)] -> [Task]
getTasks confitems =
	getTasks' confitems 1
	where
		getTasks' :: [(CF.OptionSpec, String)] -> Int -> [Task]
		getTasks' items i =
			if (lookup ("keyword" ++ (show i)) items) == Nothing
				then []
				else (getTask items i) : (getTasks' items (i+1))

getTask :: [(CF.OptionSpec, String)] -> Int -> Task
getTask items n =
	let
		keywordN	= "keyword" ++ (show n)
		thresholdN	= keywordN ++ "_threshold"
		actionN		= keywordN ++ "_action"
		kwd 		= fromJust (lookup keywordN items)
		thd 		= read $ fromJust (lookup thresholdN items)
		act 		= fromJust (lookup actionN items)
	in
		Task kwd thd act

getUrls :: FilePath -> IO [String]
getUrls path = do
	s <- readFile path
	return $ lines s
