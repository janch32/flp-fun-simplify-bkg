-- Název: simplify-bkg
-- Popis: Funkcionální projekt do předmětu FLP 2021
--
-- Autor: Jan Chaloupka (xchalo16)
-- Datum: 2021-04-11

-- | Hlavní číst programu, zpracování argumentů a vypsání gramatiky
module Main where

import System.Environment ( getArgs )
import System.IO
import Parse
import Common
import Simplify

-- | Převod interní reprezentace gramatiky na uživatelem čitelný výstup
grammarToStr :: Grammar -> String
grammarToStr (Grammar nt t first rules) =
    symbolsToStr nt ++ "\n" ++ symbolsToStr t ++ ['\n', first,'\n'] ++ rulesToStr rules

-- | Převod seznamu symbolů (termů a netermů) na uživatelem čitelný výstup
symbolsToStr :: [Symbol] -> String
symbolsToStr [x] = [x] -- aby se na konec nepřidala čárka
symbolsToStr (x:xs) = x : ',' : symbolsToStr xs
symbolsToStr [] = ""

-- | Převod pravidel gramatiky na uživatelem čitelný výstup
rulesToStr :: [Rule] -> String
rulesToStr (Rule nt []:xs) = nt : "->#\n" ++ rulesToStr xs
rulesToStr (Rule nt s:xs) = nt : "->" ++ s ++ "\n" ++ rulesToStr xs
rulesToStr [] = []


-- | Analýza argumentů, načtení vstupní gramatiky a spuštění zpracování
parseArgs :: [String] -> IO String
parseArgs [mode] = do
    content <- getContents
    return $ simplifyBkg mode content
parseArgs [mode, file] = do
    handle <- openFile file ReadMode
    content <- hGetContents handle
    return $ simplifyBkg mode content
parseArgs args = error "Wrong number of arguments. Run with -h to show help"

-- | Zpracování argumentu a spuštění požadované části programu
simplifyBkg :: String -> String -> String
simplifyBkg "-h" handle = "Usage: simplify-bkg -i12 [file]\n"
simplifyBkg "-i" handle = grammarToStr $ parseFromStr handle
simplifyBkg "-1" handle = grammarToStr $ firstStep $ parseFromStr handle
simplifyBkg "-2" handle = grammarToStr $ simplifyFull $ parseFromStr handle
simplifyBkg f _ = "Unknown argument: " ++ f ++ ". Run with -h to show help\n"

-- | Hlavní vstup programu - načte argumenty, zpracuje je a vypíše výsledek
main :: IO ()
main = getArgs >>= parseArgs >>= putStr
