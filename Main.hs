import Control.Monad (when)
import System.Environment
import System.Console.GetOpt()

import Conf
import Daemon

main :: IO ()
main = do
    parsedArgv <- getArgs >>= parseArgv
    let parsedOptions = fst parsedArgv
    createDotDir
    when (optVerbose parsedOptions) (print parsedOptions)
    conf <- buildConf parsedOptions
    when (optPrint $ opts conf) (getAndPrintHeadlines conf)
    when (optCount $ opts conf) (countAndPrint conf (optKeywords parsedOptions))
    when (optTasks $ opts conf) (runTasks conf)

