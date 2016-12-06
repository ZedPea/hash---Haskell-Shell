import System.Directory (doesDirectoryExist, doesFileExist,
                            setCurrentDirectory, getHomeDirectory)
import Text.Printf (printf)
import System.Process (proc, createProcess, waitForProcess)
import System.IO (hFlush, stdout, isEOF)

main :: IO ()
main = do
    loadConfig
    loop
    shutdown

loadConfig :: IO ()
loadConfig = return ()

shutdown :: IO ()
shutdown = return ()

loop :: IO ()
loop = do
    putStr "> "
    hFlush stdout
    end <- isEOF
    if end
        then putStr "\n"
        else do
    prog <- words <$> getLine
    status <- execute prog
    case status of
        0 -> loop
        _ -> return ()

execute :: [String] -> IO Int
execute [] = exit
execute (name:args)
    | name == "cd" = go $ cd args
    | name == "exit" = exit
    | name == "help" = go help
    | otherwise = go $ launch name args
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
    waitForProcess p
    return ()
