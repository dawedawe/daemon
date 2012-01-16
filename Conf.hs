module Conf
( Conf (..)
, buildConf
) where

import qualified Data.ConfigFile as CF
import Data.Either.Utils (forceEither)
import Network.Browser

data Conf = Conf {
	proxy :: Proxy,
	urls :: [String]
}

buildConf :: FilePath -> FilePath -> IO Conf
buildConf confpath feedpath = do
	p <- getProxyConf confpath
	feedurls <- getUrls feedpath
	return $ Conf p feedurls 

getProxyConf :: FilePath -> IO Proxy
getProxyConf path = do
	val <- CF.readfile CF.emptyCP path
	let cp = forceEither val
	let confitems = forceEither $ CF.items cp "DEFAULT"
	return $ mayToProxy $ lookup "proxy" confitems
	where
		mayToProxy :: Maybe String -> Proxy
		mayToProxy Nothing = NoProxy
		mayToProxy (Just s) = Proxy s Nothing

getUrls :: FilePath -> IO [String]
getUrls path = do
	s <- readFile path
	return $ lines s


