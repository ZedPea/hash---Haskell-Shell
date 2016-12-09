import System.Directory (doesDirectoryExist, doesFileExist,
                            setCurrentDirectory, getHomeDirectory)
import Text.Printf (printf)
import System.Process (proc, createProcess, waitForProcess)
import System.IO (hFlush, stdout, isEOF)
import System.FilePath.Posix ((</>))
import Data.List (isPrefixOf, stripPrefix)
import Control.Exception (try, SomeException)
import Control.Monad (void)
import Text.Regex.TDFA ((=~))
import Data.Maybe (fromJust)

data Config = Config {
    prompt :: String,
    aliases :: [Alias]
}

data Alias = Alias {
    abbrev :: String,
    command :: String
}

main :: IO ()
main = do
    config <- loadConfig
    loop config
    shutdown

loadConfig :: IO Config
loadConfig = do
    home <- getHomeDirectory
    let file = home </> "hash.rc"
    exists <- doesFileExist file
    if exists
        then parseConfig file
        else return defaultConfig

defaultConfig :: Config
defaultConfig = Config {
    prompt = "> ",
    aliases = []
}

parseConfig :: FilePath -> IO Config
parseConfig path = do
    input <- lines <$> readFile path
    let aliasLines = filter ("alias " `isPrefixOf`) input
        promptLines = filter ("prompt=" `isPrefixOf`) input
        aliases' = getAliases aliasLines
    let prompt' = getPrompt promptLines
    return Config {
        prompt = prompt',
        aliases = aliases'
    }

getAliases = undefined

getPrompt :: [String] -> String
getPrompt [x]
    | null match = error "Error parsing config file: invalid prompt definition"
    | otherwise = tail $ init $ fromJust $ stripPrefix "prompt=" match
    where match = x =~ regex
          regex = "^prompt=('.+'|\".+\")$"

getPrompt _ = error "Error parsing config file: multiple definitions of prompt"

shutdown :: IO ()
shutdown = return ()

loop :: Config -> IO ()
loop cfg = do
    putStr (prompt cfg)
    hFlush stdout
    end <- isEOF
    if end
        then putStr "\n"
        else do
    prog <- words <$> getLine
    status <- execute prog
    case status of
        0 -> loop cfg
        _ -> return ()

execute :: [String] -> IO Int
execute [] = exit
execute (name:args)
    | name == "cd" = go $ cd args
    | name == "exit" = exit
    | name == "help" = go help
    | otherwise = do
        runProc <- try (launch name args) :: IO (Either SomeException ())
        case runProc of
            Right _ -> return 0
            Left _ -> printf "hash: %s: command not found\n" name >> return 0
    where go f = f >> return 0

builtIn :: [String]
builtIn = ["cd", "exit", "help"]

cd :: [String] -> IO ()
cd [dir] = tryChangeDir dir
cd [] = do
    home <- getHomeDirectory
    tryChangeDir home
cd _ = putStrLn "cd: too many arguments"

tryChangeDir :: String -> IO ()
tryChangeDir dir = do
    isDir <- doesDirectoryExist dir
    isFile <- doesFileExist dir
    changeDir isDir isFile
    where changeDir isDir isFile
            | isDir = setCurrentDirectory dir
            | isFile = printf "cd: %s: Not a directory\n" dir
            | otherwise = printf "cd: %s: No such file or directory\n" dir

exit :: IO Int
exit = return 1

help :: IO ()
help = do
    putStrLn "hash - Haskell Shell\n"
    putStrLn "Enter program name and arguments."
    putStrLn "Built in commands:"
    mapM_ putStrLn builtIn

launch :: String -> [String] -> IO ()
launch name args = do
    (_,_,_,p) <- createProcess (proc name args)
    void $ waitForProcess p
