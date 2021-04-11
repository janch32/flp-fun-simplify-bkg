module Main where

import System.Environment ( getArgs )
import System.IO

data RunFlag = JustPrint | FirstStep | Full | Help

parseFlag :: String -> RunFlag
parseFlag "-i" = JustPrint
parseFlag "-1" = FirstStep
parseFlag "-2" = Full
parseFlag "-h" = Help
parseFlag f = error ("Unknown argument: " ++ f ++ ". Run with -h to show help")

parseArgs :: [String] -> IO String
parseArgs [mode] = getContents >>= simplifyBkg mode
parseArgs [mode, file] = do
    handle <- openFile file ReadMode
    content <- hGetContents handle
    simplifyBkg mode content
parseArgs args = error "Wrong number of arguments. Run with -h to show help"

simplifyBkg :: String -> String -> IO String
simplifyBkg "-h" handle = return "Usage: simplify-bkg -i12 [file]"
simplifyBkg "-i" handle = return handle
simplifyBkg "-1" handle = return "FirstStep"
simplifyBkg "-2" handle = return "Full"
simplifyBkg f _ = error ("Unknown argument: " ++ f ++ ". Run with -h to show help")

main = do getArgs >>= parseArgs >>= print
