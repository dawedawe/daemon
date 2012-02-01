import System.Environment
import System.Console.GetOpt

import Daemon
import Conf

data Options = Options
	{ optVerbose	:: Bool
	, optConfigPath	:: FilePath
	, optFeedsPath	:: FilePath
	, optPrint		:: Bool
	, optCount		:: Bool
	, optKeywords	:: [String]
	} deriving Show

main :: IO ()
main = do
	parsedArgv <- (getArgs >>= parseArgv)
	let parsedOptions = fst parsedArgv
	putStrLn $ "-v " ++ (show $ optVerbose parsedOptions)
	putStrLn $ "-C " ++ (optConfigPath parsedOptions)
	putStrLn $ "-f " ++ (optFeedsPath parsedOptions)
	putStrLn $ "-p " ++ (show $ optPrint parsedOptions)
	putStrLn $ "-c " ++ (show $ optCount parsedOptions)
	putStrLn $ "keywords " ++ (unwords $ optKeywords parsedOptions)

	conf <- buildConf (optConfigPath parsedOptions) (optFeedsPath parsedOptions)
	if (optPrint parsedOptions)
		then getAndPrintHeadlines conf
		else return ()
	if (optCount parsedOptions)
		then countAndPrint conf (optKeywords parsedOptions)
		else return ()

defaultOptions :: Options
defaultOptions = Options
	{ optVerbose	= False
	, optConfigPath = "./daemon.conf"
	, optFeedsPath	= "./feeds"
	, optPrint		= False
	, optCount		= False
	, optKeywords	= []
	}

options :: [OptDescr (Options -> Options)]
options =
	[ Option ['v'] ["verbose"]
			 (NoArg (\opts -> opts { optVerbose = True }))
			 "verbose output"
	, Option ['C'] ["config"]
			 (ReqArg (\p opts -> opts {optConfigPath = p }) "PATH")
			 "filepath to config"
	, Option ['f'] ["feedspath"]
			 (ReqArg (\p opts -> opts { optFeedsPath = p }) "PATH")
			 "filepath to feeds"
	, Option ['p'] ["print"]
			 (NoArg (\opts -> opts { optPrint = True }))
			 "print feeds"
	, Option ['c'] ["count"]
			 (ReqArg (\d opts -> opts { optCount = True,
			 optKeywords = optKeywords opts ++ [d] }) "KEYWORD ...")
			 "keyword to count"
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
						"[-p | -c word -c ...]"
