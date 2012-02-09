import System.Environment
import System.Console.GetOpt()

import Daemon
import Conf

main :: IO ()
main = do
	parsedArgv <- (getArgs >>= parseArgv)
	let parsedOptions = fst parsedArgv

	if (optVerbose parsedOptions)
	  then do
	    putStrLn $ "-v " ++ (show $ optVerbose parsedOptions)
	    putStrLn $ "-C " ++ (optConfigPath parsedOptions)
	    putStrLn $ "-f " ++ (optFeedsPath parsedOptions)
	    putStrLn $ "-p " ++ (show $ optPrint parsedOptions)
	    putStrLn $ "-c " ++ (show $ optCount parsedOptions)
	    putStrLn $ "keywords " ++ (unwords $ optKeywords parsedOptions)
	    putStrLn $ "-t " ++ (show $ optTasks parsedOptions)
	  else return ()

	conf <- buildConf parsedOptions
	if (optPrint $ opts $ conf)
	  then getAndPrintHeadlines conf
	  else return ()
	if (optCount $ opts $ conf)
	  then countAndPrint conf (optKeywords parsedOptions)
	  else return ()
	if (optTasks $ opts $ conf)
	  then runTasks conf
	  else return ()

