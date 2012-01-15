import System.Environment

import Daemon

main :: IO ()
main = do
	args <- getArgs
	if (length args) == 0
		then usage
		else do
				urls <- getUrls $ args !! 0
				case args of
					[_, "print"] -> getAndPrintHeadlines urls
					(_:"count": keywords) -> countAndPrint urls keywords
					_ -> usage

usage :: IO ()
usage = putStrLn "daemon path [print | count words ...]"
