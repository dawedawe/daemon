import System.Environment
import System.Console.GetOpt()

import Daemon
import Conf

main :: IO ()
main = do
	parsedArgv <- (getArgs >>= parseArgv)
	let parsedOptions = fst parsedArgv

	if (optVerbose parsedOptions)
	  then putStrLn $ show parsedOptions
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

