import System.Directory (doesDirectoryExist, doesFileExist,
                            setCurrentDirectory, getHomeDirectory)
import Text.Printf (printf)
import System.Process (proc, createProcess, waitForProcess)
import System.IO (hFlush, stdout, isEOF)
import System.FilePath.Posix ((</>))
import Control.Exception (try, SomeException)
import Control.Monad (void, unless)
import Text.Regex.TDFA ((=~))
import Data.Maybe (mapMaybe)

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
    prompt = defPrompt,
    aliases = []
}

defPrompt :: String
defPrompt = "> "

parseConfig :: FilePath -> IO Config
parseConfig path = do
    input <- lines <$> readFile path
    let aliases' = mapMaybe getAlias $ concatMap (=~ aliasregex) input
        prompt' = getPrompt $ concatMap (=~ promptregex) input
    return $ Config prompt' aliases'
    where aliasregex = "^alias (.+)=\'(.+)\'$"
          promptregex = "^prompt=\'(.+)\'"

getPrompt :: [[String]] -> String
getPrompt p
    | not $ null p && length (head p) == 2 = head p !! 1
    | otherwise = defPrompt

getAlias :: [String] -> Maybe Alias
getAlias input
    | length input == 3 = Just $ Alias (input !! 1) (input !! 2)
    | otherwise = Nothing

shutdown :: IO ()
shutdown = return ()

{- exception from hGetContents if EOF inputted and don't check for it, EOF
also closes the program, presumably exiting to another shell, so print a 
newline so it looks more graceful -}
loop :: Config -> IO ()
loop cfg = do
    putStr (prompt cfg)
    hFlush stdout
    end <- isEOF
    if end
        then putStr "\n"
        else do
    prog <- map (convert cfg) . words <$> getLine
    unless (not (null prog) && head prog == "exit") $ do
        execute prog cfg 
        loop cfg

execute :: [String] -> Config -> IO ()
execute [] _ = return ()
execute ("cd":args) _ = cd args
execute ("help":_) _ = help
execute ("alias":_) cfg = alias cfg
execute (name:args) _ = do
    runProc <- try (launch name args) :: IO (Either SomeException ())
    case runProc of
        Right _ -> return ()
        Left _ -> printf "hash: %s: command not found\n" name

convert :: Config -> String -> String
convert cfg input = replace (aliases cfg)
    where replace [] = input
          replace (x:xs)
            | abbrev x == input = command x
            | otherwise = replace xs

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
