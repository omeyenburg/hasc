module Main where

import System.Environment (getArgs)

data ParsedArgs = ParsedArgs
  { special :: String,
    options :: [String],
    files :: [String]
  }
  deriving (Show)

countLines :: String -> Int
countLines "" = 0
countLines str = (if head str == '\n' then 1 else 0) + countLines (tail str)

analyzeFile :: String -> IO ()
analyzeFile fileName = do
  content <- readFile fileName
  let charCount = length content
      wordCount = length $ words content
      lineCount = countLines content

  -- 3  6 16 test.txt
  putStrLn ("Line count: " ++ show lineCount)
  putStrLn ("Word count: " ++ show wordCount)
  putStrLn ("Char count: " ++ show charCount)

parseArgs :: [String] -> ParsedArgs
parseArgs [] = ParsedArgs {special = "", options = [], files = []}
parseArgs (arg : args)
  | arg == "--" = ParsedArgs {special = "", options = [], files = args} -- After "--", treat all further arguments as files
  | head arg == '-' = do
      let parsed = parseArgs args

      case arg of
        "--help" -> ParsedArgs {special = "help", options = [], files = []}
        "--version" -> ParsedArgs {special = "version", options = [], files = []}
        "-" -> ParsedArgs {special = special parsed, options = options parsed, files = files parsed ++ ["-"]} -- Treat "-" as a file
        "--bytes" -> ParsedArgs {special = special parsed, options = options parsed ++ ["bytes"], files = files parsed}
        "--chars" -> ParsedArgs {special = special parsed, options = options parsed ++ ["chars"], files = files parsed}
        "--lines" -> ParsedArgs {special = special parsed, options = options parsed ++ ["lines"], files = files parsed}
        "--max-line-length" -> ParsedArgs {special = special parsed, options = options parsed ++ ["max-line-length"], files = files parsed}
        "--words" -> ParsedArgs {special = special parsed, options = options parsed ++ ["words"], files = files parsed}
        _ ->
          if all (`elem` "cmlLw") (tail arg)
            then parsed {options = options parsed ++ map parse (tail arg)}
            else parsed {special = arg}
          where
            parse 'c' = "bytes"
            parse 'm' = "chars"
            parse 'l' = "lines"
            parse 'L' = "max-line-length"
            parse 'w' = "words"
            parse _ = "unexpected"
  | otherwise = do
      let parsed = parseArgs args
      ParsedArgs {special = special parsed, options = options parsed, files = files parsed ++ [arg]}

analyzeFiles :: [String] -> [String] -> String
analyzeFiles opts (file:files) = show opts
analyzeFiles opts [] = show opts

main :: IO ()
main = do
  -- let fileName = "test.txt"
  -- analyzeFile fileName

  args <- getArgs
  let parsed = parseArgs args
  case special parsed of
    "help" -> putStrLn helpString
    "version" -> putStrLn versionString
    "" -> putStrLn $ analyzeFiles (options parsed) (files parsed)
    _ -> putStrLn $ invalidString $ special parsed

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
