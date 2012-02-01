module Conf
( Conf (..)
, Options (..)
, Task (..)
, buildConf
, parseArgv
) where

import qualified Data.ConfigFile as CF
import Data.Either.Utils (forceEither)
import Network.Browser
import System.Console.GetOpt

data Conf = Conf
	{ opts		:: Options
	, proxy		:: Proxy
	, urls		:: [String]
	, tasks		:: [Task] }

data Task = Task
	{ keyword	:: String
	, threshold	:: Int
	, action	:: String }

data Options = Options
	{ optVerbose	:: Bool
	, optConfigPath	:: FilePath
	, optFeedsPath	:: FilePath
	, optPrint		:: Bool
	, optCount		:: Bool
	, optKeywords	:: [String]
	, optTasks		:: Bool
	} deriving Show

defaultOptions :: Options
defaultOptions = Options
	{ optVerbose	= False
	, optConfigPath = "./daemon.conf"
	, optFeedsPath	= "./feeds"
	, optPrint		= False
	, optCount		= False
	, optKeywords	= []
	, optTasks		= False
	}

options :: [OptDescr (Options -> Options)]
options =
	[ Option ['v'] ["verbose"]
			 (NoArg (\optns -> optns { optVerbose = True }))
			 "verbose output"
	, Option ['C'] ["config"]
			 (ReqArg (\p optns -> optns {optConfigPath = p }) "PATH")
			 "filepath to config"
	, Option ['f'] ["feedspath"]
			 (ReqArg (\p optns -> optns { optFeedsPath = p }) "PATH")
			 "filepath to feeds"
	, Option ['p'] ["print"]
			 (NoArg (\optns -> optns { optPrint = True }))
			 "print feeds"
	, Option ['c'] ["count"]
			 (ReqArg (\d optns -> optns { optCount = True,
			 optKeywords = optKeywords optns ++ [d] }) "KEYWORD ...")
			 "keyword to count"
	, Option ['t'] ["tasks"]
			 (NoArg (\optns -> optns { optTasks = True }))
			 "run tasks"
	]

parseArgv :: [String] -> IO (Options, [String]) 
parseArgv argv =
	let
		opt = getOpt RequireOrder options argv
	in
		case opt of
			(o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
			(_,_,errs) -> ioError (userError
							(concat errs ++ usageInfo header options))
      	where header = "Usage: daemon [-v] [-f feedspath] " ++
						"[-p | -c word -c ... | -t]"

buildConf :: Options -> IO Conf
buildConf o = do
	items		<- getConfItems $ optConfigPath o
	feedurls	<- getUrls $ optFeedsPath o
	let prx		= getProxyConf items
	let ts		= getTasks items
	return $ Conf o prx feedurls ts

getConfItems :: FilePath -> IO [(CF.OptionSpec, String)]
getConfItems path = do
	val		<- CF.readfile CF.emptyCP path
	let cp	= forceEither val
	return $ forceEither $ CF.items cp "DEFAULT"

getProxyConf :: [(CF.OptionSpec, String)] -> Proxy
getProxyConf items = do
	maybeToProxy $ lookup "proxy" items
	where
		maybeToProxy :: Maybe String -> Proxy
		maybeToProxy Nothing  = NoProxy
		maybeToProxy (Just s) = Proxy s Nothing

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
		kwd 		= checkConfItem (lookup keywordN items) keywordN 
		thd 		= read $ checkConfItem (lookup thresholdN items) thresholdN
		act 		= checkConfItem (lookup actionN items) actionN
	in
		Task kwd thd act

getUrls :: FilePath -> IO [String]
getUrls path = readFile path >>= (return . lines)

checkConfItem :: Maybe String -> String -> String
checkConfItem (Just s) _ = s
checkConfItem Nothing itemName = error $ "failed to parse " ++ itemName
