import System.Environment

import Daemon
import Conf

main :: IO ()
main = do
	args <- getArgs
	if (length args) == 0
		then usage
		else do
				let feedpath = (args !! 0)
				conf <- buildConf "./daemon.conf" feedpath

				case args of
					[_, "print"] -> getAndPrintHeadlines conf
					(_:"count": keywords) -> countAndPrint conf keywords
					_ -> usage

usage :: IO ()
usage = putStrLn "daemon path [print | count words ...]"
