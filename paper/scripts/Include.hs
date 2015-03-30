module Include where

import System.Directory   (copyFile)
import System.Environment (getArgs)

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe


-- Abstract syntax of a LaTeX source file
data AbstractLine = S String | D Directive
data Directive = Include FilePath Language (Maybe Tag)
type Language = String
type Tag      = String

type Processor = String -> IO String


-- | A LaTeX preprocessor.
latexP :: Processor
latexP = emit . parse . lines


-- | Parse in default state.
parse :: [String] -> [AbstractLine]
parse []       = []
parse (s:rest) =
  case stripPrefix "% #include " s of
    Nothing       -> S s : parse rest
    Just argsPart -> D d : skipToEnd rest
                       where d = parseIncludeArgsPart argsPart

-- | Skip to the next end and then resume parsing.
skipToEnd :: [String] -> [AbstractLine]
skipToEnd [] = error "Still expecting an #end"
skipToEnd (s:rest) =
  case stripPrefix "% #end" s of
    Nothing -> skipToEnd rest
    Just _  -> parse rest

parseIncludeArgsPart :: String -> Directive
parseIncludeArgsPart argsPart =
  if length args >= 3
     then Include (args !! 0) (args !! 1) (Just (drop 1 (args !! 2)))
     else Include (args !! 0) (args !! 1) Nothing
  where args = words argsPart


-- | Emit output string based on abstract lines.
emit :: [AbstractLine] -> IO String
emit = liftM unlines . mapM emitOne

emitOne :: AbstractLine -> IO String
emitOne (S s) = return s
emitOne (D (Include filePath language maybeTag)) =
  do body <- case maybeTag of
               Nothing  -> fetchFile        filePath
               Just tag -> fetchFileSection filePath tag
     return $
       unlines [ unwords ["% #include", filePath, language, case maybeTag of { Nothing -> ""; Just tag -> "@" ++ tag}]
               , "\\begin{minted}{" ++ language ++ "}"
               , body
               , "\\end{minted}"
               , "% #end"]


fetchFile :: FilePath -> IO String
fetchFile filePath =
  do putStrLn ("Fetching " ++ filePath)
     liftM strip (readFile filePath)

fetchFileSection :: FilePath -> Tag -> IO String
fetchFileSection filePath tag =
  do putStrLn ("Fetching section " ++ tag ++ " from " ++ filePath)
     content <- readFile filePath
     let (from, to) = locate tag content
     return $ (strip . unlines . slice (from, to) . lines) content

-- | Locate a tagged section in a file. Returns the first line number and the
-- last line number of that section.
locate :: Tag -> String -> (Int, Int)
locate tag s =
  let tagLineNos = map (+1) $ findIndices (("-- @" ++ tag) `isPrefixOf`) (lines s) in
                                          -- FIXME: If one tag is the prefix of another...
  case tagLineNos of
    []          -> error ("tag not found: " ++ tag)
    [tagLineNo] ->
      let endLineNos = map (+1) $ findIndices (\l -> "-- @" == rstrip l) (lines s) in
      (tagLineNo + 1, head (filter (> tagLineNo) endLineNos) - 1)
    _          -> error ("duplicate tag: " ++ tag)

-- | Slice lines according to the line numbers.
slice :: (Int, Int) -> [String] -> [String]
slice (from, to) = take (to - from + 1) . drop (from - 1)


-- | Strippers.
lstrip, rstrip, strip :: String -> String
lstrip = dropWhile isSpace
rstrip = dropWhileEnd isSpace
strip  = rstrip . lstrip


-- | Run the given file processor on the file path.
run :: Processor -> FilePath -> IO ()
run process filePath =
  do putStrLn ("Processing " ++ filePath)
     -- Try to backup the old file first
     copyFile filePath (filePath ++ "-old")

     -- Read and process the file
     readFile filePath >>= process >>= writeFile filePath


main :: IO ()
main =
  do args <- getArgs
     let filePaths = if length args < 1 then ["main.tex"] else args
     mapM_ (run latexP) filePaths
