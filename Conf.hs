module Conf
( Conf (..)
, Options (..)
, Task (..)
, buildConf
, createDotDir
, parseArgv
) where

import Control.Monad (unless)
import qualified Data.ConfigFile as CF
import Data.Either.Utils (forceEither)
import Data.Maybe (isNothing)
import Network.Browser
import System.Console.GetOpt
import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
    getAppUserDataDirectory)
import System.FilePath (pathSeparator)

data Conf = Conf {
            opts      :: Options
          , proxy     :: Proxy
          , urls      :: [String]
          , tasks     :: [Task]
          }

data Task = Task {
            keyword   :: String
          , threshold :: Int
          , action    :: String
          }

data Options = Options {
               optVerbose    :: Bool
             , optConfigPath :: FilePath
             , optFeedsPath  :: FilePath
             , optPrint      :: Bool
             , optCount      :: Bool
             , optKeywords   :: [String]
             , optTasks      :: Bool
             }

instance Show Options where
    show o =
      "-v " ++ show (optVerbose o) ++ "\n" ++
      "-C " ++ optConfigPath o ++ "\n" ++
      "-f " ++ optFeedsPath o ++ "\n" ++
      "-p " ++ show (optPrint o) ++ "\n" ++
      "-c " ++ show (optCount o) ++ "\n" ++
      "keywords " ++ unwords (optKeywords o) ++ "\n" ++
      "-t " ++ show (optTasks o)

-- |Default cli options, used if none given.
defaultOptions :: FilePath -> Options
defaultOptions p = Options {
      optVerbose    = False
    , optConfigPath = p ++ [pathSeparator] ++ "daemon.conf"
    , optFeedsPath  = p ++ [pathSeparator] ++ "daemon.feeds"
    , optPrint      = False
    , optCount      = False
    , optKeywords   = []
    , optTasks      = False
    }

options :: [OptDescr (Options -> Options)]
options = [
      Option "v" ["verbose"]
      (NoArg (\optns -> optns { optVerbose = True }))
      "verbose output"
    , Option "C" ["config"]
      (ReqArg (\p optns -> optns {optConfigPath = p }) "PATH")
      "filepath to config"
    , Option "f" ["feedspath"]
      (ReqArg (\p optns -> optns { optFeedsPath = p }) "PATH")
      "filepath to feeds"
    , Option "p" ["print"]
      (NoArg (\optns -> optns { optPrint = True }))
      "print feeds"
    , Option "c" ["count"]
      (ReqArg (\d optns -> optns { optCount = True,
      optKeywords = optKeywords optns ++ [d] }) "KEYWORD ...")
      "keyword to count"
    , Option "t" ["tasks"]
      (NoArg (\optns -> optns { optTasks = True }))
      "run tasks"
    ]

-- |Parse the cli argument vector.
parseArgv :: [String] -> IO (Options, [String]) 
parseArgv argv = do
    let opt = getOpt RequireOrder options argv
    dDir <- dotDirPath 
    case opt of
          (o,n,[]  ) -> return (foldl (flip id) (defaultOptions dDir) o, n)
          (_,_,errs) -> ioError
            (userError (concat errs ++ usageInfo header options))
            where header = "Usage: daemon [-v] [-f feedspath] " ++
                    "[-p | -c word -c ... | -t]"

-- |Build Conf out of cli options and config file definitions.
buildConf :: Options -> IO Conf
buildConf o = do
    items    <- getConfItems $ optConfigPath o
    feedurls <- getUrls $ optFeedsPath o
    let prx  = getProxyConf items
    let ts   = getTasks items
    return $ Conf o prx feedurls ts

-- |Read config items out of config file.
getConfItems :: FilePath -> IO [(CF.OptionSpec, String)]
getConfItems path = do
    val    <- CF.readfile CF.emptyCP path
    let cp = forceEither val
    return $ forceEither $ CF.items cp "DEFAULT"

getProxyConf :: [(CF.OptionSpec, String)] -> Proxy
getProxyConf items = maybeToProxy $ lookup "proxy" items

maybeToProxy :: Maybe String -> Proxy
maybeToProxy Nothing  = NoProxy
maybeToProxy (Just s) = Proxy s Nothing

getTasks :: [(CF.OptionSpec, String)] -> [Task]
getTasks confitems =
    getTasks' confitems 1
    where
      getTasks' :: [(CF.OptionSpec, String)] -> Int -> [Task]
      getTasks' items i =
        if isNothing (lookup ("keyword" ++ show i) items)
          then []
          else getTask items i : getTasks' items (i+1)

-- |Construct a Task out of the config file definitions
getTask :: [(CF.OptionSpec, String)] -> Int -> Task
getTask items n =
    let
      keywordN   = "keyword" ++ show n
      thresholdN = keywordN ++ "_threshold"
      actionN    = keywordN ++ "_action"
      kwd        = checkConfItem (lookup keywordN items) keywordN 
      thd        = read $ checkConfItem (lookup thresholdN items) thresholdN
      act        = checkConfItem (lookup actionN items) actionN
    in Task kwd thd act

-- |Read the urls of the feeds file
getUrls :: FilePath -> IO [String]
getUrls = fmap lines . readFile

checkConfItem :: Maybe String -> String -> String
checkConfItem (Just s) _       = s
checkConfItem Nothing itemName = error $ "failed to parse " ++ itemName

-- |Create dotdir with a default config file.
createDotDir :: IO ()
createDotDir = do
    dDir   <- dotDirPath
    exists <- doesDirectoryExist dDir
    createDirectoryIfMissing (not exists) dDir
    let cPath = dDir ++ [pathSeparator] ++ "daemon.conf"
    let fPath = dDir ++ [pathSeparator] ++ "daemon.feeds"
    unless exists $ do
                      writeFile cPath defaultConf
                      writeFile fPath defaultFeeds

-- |Default path to application data directory aka the dotdir.
dotDirPath :: IO String
dotDirPath = getAppUserDataDirectory "daemon"

-- |Default configuration with proxy and keyword example.
defaultConf :: String
defaultConf =
    "#proxy = 127.0.0.1:8118\n" ++
    "#keyword1 = nasa\n" ++
    "#keyword1_threshold = 1\n" ++
    "#keyword1_action = xeyes\n" ++
    "#keyword2 = mars\n" ++
    "#keyword2_threshold = 1\n" ++
    "#keyword2_action = xeyes"

-- |Default feeds
defaultFeeds :: String
defaultFeeds = "http://feeds.bbci.co.uk/news/rss.xml"
