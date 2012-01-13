import Data.Char
import qualified Data.Text as T
import Network.HTTP
import System.Environment
import Text.HTML.TagSoup

import Daemon

main :: IO ()
main = do
	args <- getArgs
	if (length args) == 0
		then usage
		else do
				urls <- getUrls $ args !! 0
				case args of
					[path, "print"] -> getAndPrintHeadlines urls
					(path:"count": keywords) -> countAndPrint urls keywords
					_ -> usage
