module Control where

import qualified FEST as F
import qualified ParseFEST as FP
import qualified BEST as B
import Text.Parsec

compile :: String -> IO (Maybe B.Expression)
compile file = do
  parseResult <- readFile file >>= parseProgram
  case parseResult of
    Left e -> do
      print e
      return Nothing
    Right fest -> 
      

parseProgram :: String -> Either ParseError [F.Declaration]
parseProgram = FP.iParse FP.sourceFile

simplify :: [F.Declaration]
