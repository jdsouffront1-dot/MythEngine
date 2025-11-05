module Verifier where

import System.IO
import Data.List (isPrefixOf)
import Text.Printf (printf)

-- | Validate the metrics file and produce a refined version
verifyAndRefine :: FilePath -> FilePath -> IO ()
verifyAndRefine inputFile outputFile = do
    contents <- readFile inputFile
    let ls = lines contents
        header = head ls
        rows = drop 1 ls
        validRows = filter validLine rows
        total = length validRows

    writeFile outputFile (unlines (header : validRows))
    printf "Phase 4.1: Verification completed. %d valid entries written to %s\n" total outputFile

-- | Simple sanity check for numeric columns
validLine :: String -> Bool
validLine line =
    let parts = splitBy ',' line
    in length parts >= 7 && all isNumeric (drop 2 parts)
  where
    splitBy :: Char -> String -> [String]
    splitBy d s = foldr (\c acc -> if c == d then [] : acc else (c : head acc) : tail acc) [[]] s

    isNumeric :: String -> Bool
    isNumeric [] = False
    isNumeric ('-':xs) = all (`elem` "0123456789.eE") xs
    isNumeric xs = all (`elem` "0123456789.eE") xs
