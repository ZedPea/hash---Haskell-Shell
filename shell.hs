import System.Directory (doesDirectoryExist, doesFileExist,
                            setCurrentDirectory, getHomeDirectory)
import Text.Printf (printf)
import System.Process (proc, createProcess, waitForProcess)
import System.IO (hFlush, stdout, isEOF)
import System.FilePath.Posix ((</>))
import Data.List (stripPrefix)
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
    let file = home </> ".hashrc"
    exists <- doesFileExist file
    if exists
        then parseConfig file
        else return defaultConfig

defaultConfig :: Config
defaultConfig = Config {
    prompt = "> ",
    aliases = []
}

defPrompt :: String
defPrompt = "> "

parseConfig :: FilePath -> IO Config
parseConfig path = do
    input <- readFile path
    let aliasLines = map head $ input =~ aliasregex
        prompt' = getPrompt $ input =~ promptregex
        aliases' = map parseAlias aliasLines
    if null prompt'
        then return $ makeConfig defPrompt aliases'
        else return $ makeConfig prompt' aliases'
    where aliasregex = "^alias [A-Za-z]+=(\"[A-Za-z]+\"|'[A-Za-z]+')$"
          promptregex = "^prompt=('.+'|\".+\")$"

makeConfig :: String -> [Alias] -> Config
makeConfig p a = Config {
    prompt = p,
    aliases = a
}

parseAlias :: String -> Alias
parseAlias a = Alias { abbrev = abbrev', command = command' }
    where abbrev' = takeWhile (/= '=') . fromJust $ stripPrefix "alias " a
          command' = tail . init . fromJust $ stripPrefix toStrip a
          toStrip = "alias " ++ abbrev' ++ "="

getPrompt :: String -> String
getPrompt p
    | null p = defPrompt
    | otherwise = tail . init . fromJust $ stripPrefix "prompt=" p

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
    prog <- convertFromAlias cfg . words <$> getLine
    status <- execute prog cfg
    case status of
        0 -> loop cfg
        _ -> return ()

execute :: [String] -> Config -> IO Int
execute [] _ = return 0
execute (name:args) cfg
    | name == "cd" = go $ cd args
    | name == "exit" = exit
    | name == "help" = go help
    | name == "alias" = alias cfg >> return 0
    | otherwise = do
        runProc <- try (launch name args) :: IO (Either SomeException ())
        case runProc of
            Right _ -> return 0
            Left _ -> printf "hash: %s: command not found\n" name >> return 0
    where go f = f >> return 0

convertFromAlias :: Config -> [String] -> [String]
convertFromAlias _ [] = []
convertFromAlias cfg (x:xs) = let c = conv $ aliases cfg
                              in  c:xs
    where conv [] = x
          conv (a:as)
            | abbrev a == x = command a
            | otherwise = conv as

builtIn :: [String]
builtIn = ["cd", "exit", "help", "alias"]

alias :: Config -> IO ()
alias cfg = mapM_ prettyprint (aliases cfg)
    where prettyprint a = printf "alias %s='%s'\n" (abbrev a) (command a)

cd :: [String] -> IO ()
cd ['~':rest] = do
    home <- getHomeDirectory
    tryChangeDir (home ++ rest)
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
