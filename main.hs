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
		else return ()

	conf <- buildConf parsedOptions
	if (optPrint parsedOptions)
		then getAndPrintHeadlines conf
		else return ()
	if (optCount parsedOptions)
		then countAndPrint conf (optKeywords parsedOptions)
		else return ()

