module Main where

import Control.Monad (when)
import Data.Char.WCWidth (wcwidth)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

data ParsedArgs = ParsedArgs
  { argSpecial :: String,
    argOptions :: [String],
    argFiles :: [String]
  }
  deriving (Show)

data ParsedFile = ParsedFile
  { fileName :: String,
    fileLines :: Int,
    fileWords :: Int,
    fileChars :: Int,
    fileBytes :: Int,
    fileMaxLineLength :: Int,
    fileMaxWidth :: Int,
    fileFlag :: Int
  }
  deriving (Show)

fileExistsFlag :: Int
fileExistsFlag = 0

directoryExistsFlag :: Int
directoryExistsFlag = 1

fileMissingFlag :: Int
fileMissingFlag = 2

contains :: [String] -> String -> Bool
contains [] _ = False
contains (x : xs) value = (x == value) || contains xs value

countLines :: String -> Int
countLines "" = 0
countLines str = (if head str == '\n' then 1 else 0) + countLines (tail str)

countBytes :: String -> Int
countBytes str =
  let text = Text.pack str -- Convert String to Text
      utf8Bytes = Encoding.encodeUtf8 text -- Encode Text to ByteString in UTF-8
   in ByteString.length utf8Bytes -- Get the length of the ByteString

getMaxLineLength :: Int -> Int -> String -> Int
getMaxLineLength m s str
  | null str = max m s
  | head str == '\n' = getMaxLineLength (max m s) 0 $ tail str
  | otherwise = getMaxLineLength (max m (s + wcwidth (head str))) (s + wcwidth (head str)) (tail str)

getNumberWidth :: Int -> Int
getNumberWidth num = do
  let numStr = show num
  length numStr

analyzeFile :: [String] -> String -> IO ParsedFile
analyzeFile opts file = do
  validFile <- doesFileExist file
  if null file || validFile
    then do
      content <- if null file then getContents else readFile file
      let lineCount = length $ lines content
          wordCount = length $ words content
          charCount = length content
          byteCount = countBytes content
          maxLineLength = getMaxLineLength 0 0 content
          fileStats = [lineCount | contains opts "lines"] ++ [wordCount | contains opts "words"] ++ [charCount | contains opts "chars"] ++ [byteCount | contains opts "bytes"] ++ [maxLineLength | contains opts "max-line-length"]
          width = getNumberWidth $ maximum fileStats + 1
      return
        ParsedFile
          { fileName = file,
            fileLines = lineCount,
            fileWords = wordCount,
            fileChars = charCount,
            fileBytes = byteCount,
            fileMaxLineLength = maxLineLength,
            fileMaxWidth = if null file then max width 7 else width,
            fileFlag = fileExistsFlag
          }
    else do
      validDirectory <- doesDirectoryExist file
      return
        ParsedFile
          { fileName = file,
            fileLines = 0,
            fileWords = 0,
            fileChars = 0,
            fileBytes = 0,
            fileMaxLineLength = 0,
            fileMaxWidth = if validDirectory then 7 else 1,
            fileFlag = if validDirectory then directoryExistsFlag else fileMissingFlag
          }

parseArgs :: [String] -> ParsedArgs
parseArgs [] = ParsedArgs {argSpecial = "", argOptions = [], argFiles = []}
parseArgs (arg : args)
  | arg == "--" = ParsedArgs {argSpecial = "", argOptions = [], argFiles = args} -- After "--", treat all further arguments as files
  | head arg == '-' = do
      let parsed = parseArgs args

      case arg of
        "--help" -> ParsedArgs {argSpecial = "help", argOptions = [], argFiles = []}
        "--version" -> ParsedArgs {argSpecial = "version", argOptions = [], argFiles = []}
        "-" -> ParsedArgs {argSpecial = argSpecial parsed, argOptions = argOptions parsed, argFiles = argFiles parsed ++ ["-"]} -- Treat "-" as a file
        "--bytes" -> ParsedArgs {argSpecial = argSpecial parsed, argOptions = argOptions parsed ++ ["bytes"], argFiles = argFiles parsed}
        "--chars" -> ParsedArgs {argSpecial = argSpecial parsed, argOptions = argOptions parsed ++ ["chars"], argFiles = argFiles parsed}
        "--lines" -> ParsedArgs {argSpecial = argSpecial parsed, argOptions = argOptions parsed ++ ["lines"], argFiles = argFiles parsed}
        "--max-line-length" -> ParsedArgs {argSpecial = argSpecial parsed, argOptions = argOptions parsed ++ ["max-line-length"], argFiles = argFiles parsed}
        "--words" -> ParsedArgs {argSpecial = argSpecial parsed, argOptions = argOptions parsed ++ ["words"], argFiles = argFiles parsed}
        _ ->
          if all (`elem` "cmlLw") (tail arg)
            then parsed {argOptions = argOptions parsed ++ map parse (tail arg)}
            else parsed {argSpecial = arg}
          where
            parse 'c' = "bytes"
            parse 'm' = "chars"
            parse 'l' = "lines"
            parse 'L' = "max-line-length"
            parse 'w' = "words"
            parse _ = "unexpected"
  | otherwise = do
      let parsed = parseArgs args
      ParsedArgs {argSpecial = argSpecial parsed, argOptions = argOptions parsed, argFiles = argFiles parsed ++ [arg]}

analyzeFiles :: [String] -> [String] -> IO [ParsedFile]
analyzeFiles opts (x : xs) = do
  result <- analyzeFile opts x
  others <- analyzeFiles opts xs
  return (result : others)
analyzeFiles _ [] = return []

addLists :: [Int] -> [Int] -> [Int]
addLists [] y = y
addLists x [] = x
addLists (x : xs) (y : ys) = x + y : addLists xs ys

outputNumber :: Int -> Int -> IO ()
outputNumber num width = do
  let numStr = show num
      padding = replicate (width - length numStr) ' '
  putStr (padding ++ numStr ++ " ")

outputFile :: [String] -> [ParsedFile] -> Int -> IO ()
outputFile _ [] _ = return ()
outputFile opts (parsed : others) width = do
  outputFile opts others width
  if fileFlag parsed == fileExistsFlag
    then do
      when (contains opts "lines") $ outputNumber (fileLines parsed) width
      when (contains opts "words") $ outputNumber (fileWords parsed) width
      when (contains opts "chars") $ outputNumber (fileChars parsed) width
      when (contains opts "bytes") $ outputNumber (fileBytes parsed) width
      when (contains opts "max-line-length") $ outputNumber (fileMaxLineLength parsed) width
      putStrLn $ fileName parsed
    else
      if fileFlag parsed == directoryExistsFlag
        then do
          when (contains opts "lines") $ outputNumber (fileLines parsed) width
          when (contains opts "words") $ outputNumber (fileWords parsed) width
          when (contains opts "chars") $ outputNumber (fileChars parsed) width
          when (contains opts "bytes") $ outputNumber (fileBytes parsed) width
          when (contains opts "max-line-length") $ outputNumber (fileMaxLineLength parsed) width
          putStrLn $ fileName parsed
          putStrLn ("wc: " ++ fileName parsed ++ ": Is a directory")
        else do
          putStrLn ("wc: " ++ fileName parsed ++ ": No such file or directory")

outputFiles :: [String] -> [ParsedFile] -> IO ()
outputFiles opts parsed = do
  let total =
        foldl1
          ( \acc file ->
              acc
                {
                  fileName = "total",
                  fileLines = fileLines acc + fileLines file,
                  fileWords = fileWords acc + fileWords file,
                  fileChars = fileChars acc + fileChars file,
                  fileBytes = fileBytes acc + fileBytes file,
                  fileMaxLineLength = max (fileMaxLineLength acc) (fileMaxLineLength file),
                  fileMaxWidth = max (fileMaxWidth acc) (fileMaxWidth file),
                  fileFlag = fileExistsFlag
                }
          )
          parsed

  -- Max width is saved in head total, to
  -- simplify proccessing with one recursive function
  outputFile opts parsed $ fileMaxWidth total

  -- Only output the total if more than one file is parsed
  when (length parsed > 1) $ outputFile opts [total] $ fileMaxWidth total

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
      opts = if argOptions parsed /= [] then argOptions parsed else ["lines", "words", "bytes"]
      fileList = if argFiles parsed /= [] then argFiles parsed else [""]
  case argSpecial parsed of
    "help" -> putStrLn helpString
    "version" -> putStrLn versionString
    "" -> do
      result <- analyzeFiles opts fileList
      outputFiles opts result
    _ -> putStrLn $ invalidString $ argSpecial parsed

invalidString :: String -> String
invalidString option
  | head (tail option) == '-' =
      "wc: invalid option -- '"
        ++ option
        ++ "'\n"
        ++ "Try 'wc --help' for more information."
  | otherwise =
      "wc: invalid option -- '"
        ++ tail option
        ++ "'\n"
        ++ "Try 'wc --help' for more information."

helpString :: String
helpString =
  "Usage: wc [OPTION]... [FILE]...\n"
    ++ "Print newline, word, and byte counts for each FILE, and a total line if\n"
    ++ "more than one FILE is specified.  A word is a nonempty sequence of non white\n"
    ++ "space delimited by white space characters or by start or end of input.\n"
    ++ "\n"
    ++ "With no FILE, or when FILE is -, read standard input.\n"
    ++ "\n"
    ++ "The options below may be used to select which counts are printed, always in\n"
    ++ "the following order: newline, word, character, byte, maximum line length.\n"
    ++ "  -c, --bytes            print the byte counts\n"
    ++ "  -m, --chars            print the character counts\n"
    ++ "  -l, --lines            print the newline counts\n"
    ++ "  -L, --max-line-length  print the maximum display width\n"
    ++ "  -w, --words            print the word counts\n"
    ++ "      --help        display this help and exit\n"
    ++ "      --version     output version information and exit"

versionString :: String
versionString =
  "wc\n"
    ++ "Copyright (C) 2025 Oskar Meyenburg.\n"
    ++ "License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>.\n"
    ++ "This is free software: you are free to change and redistribute it.\n"
    ++ "There is NO WARRANTY, to the extent permitted by law."
