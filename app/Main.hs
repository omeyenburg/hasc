module Main where

import System.Environment (getArgs)

printChars :: String -> IO ()
printChars "" = do
  putChar 'x'
printChars str = do
  putChar (head str)
  printChars (tail str)

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

parseArgs :: [String] -> [String]
parseArgs [] = []
parseArgs (arg : args) = parseArg arg : parseArgs args

parseArg :: String -> String
parseArg arg = do
  case arg of
    "--help" -> helpString
    _ -> "smth else"

main :: IO ()
main = do
  let fileName = "test.txt"

  args <- getArgs
  print (parseArgs args)
  -- case args of
  --   ["--help"] -> putStrLn helpString
  --   [arg] -> putStrLn $ "Argument: " ++ arg
  --   _ -> putStrLn "Invalid arguments"

  analyzeFile fileName

readInt :: String -> Int
readInt = read

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
    ++ "      --total=WHEN       when to print a line with total counts;\n"
    ++ "                           WHEN can be: auto, always, only, never\n"
    ++ "      --help        display this help and exit\n"
    ++ "      --version     output version information and exit"
