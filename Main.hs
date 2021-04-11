module Main where

import System.Environment ( getArgs )
import System.IO
import Parse
import Common
import Simplify

grammarToStr :: Grammar -> String
grammarToStr (Grammar nt t first rules) =
    symbolsToStr nt ++ "\n" ++ symbolsToStr t ++ ['\n', first,'\n'] ++ rulesToStr rules

symbolsToStr :: [Symbol] -> String
symbolsToStr [x] = [x] -- aby se na konec nepřidala čárka
symbolsToStr (x:xs) = x : ',' : symbolsToStr xs
symbolsToStr [] = ""

rulesToStr :: [Rule] -> String
rulesToStr (Rule nt []:xs) = nt : "->#\n" ++ rulesToStr xs
rulesToStr (Rule nt s:xs) = nt : "->" ++ s ++ "\n" ++ rulesToStr xs
rulesToStr [] = []

parseArgs :: [String] -> IO String
parseArgs [mode] = simplifyBkg mode <$> getContents
parseArgs [mode, file] = do
    handle <- openFile file ReadMode
    content <- hGetContents handle
    return $ simplifyBkg mode content
parseArgs args = error "Wrong number of arguments. Run with -h to show help"

simplifyBkg :: String -> String -> String
simplifyBkg "-h" handle = "Usage: simplify-bkg -i12 [file]\n"
simplifyBkg "-i" handle = grammarToStr $ parseFromStr handle
simplifyBkg "-1" handle = grammarToStr $ firstStep $ parseFromStr handle
simplifyBkg "-2" handle = grammarToStr $ simplifyFull $ parseFromStr handle
simplifyBkg f _ = "Unknown argument: " ++ f ++ ". Run with -h to show help\n"

main = getArgs >>= parseArgs >>= putStr
