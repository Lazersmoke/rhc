data RPS = Rock | Paper | Scissors deriving (Show,Eq)

parseRPS :: Parser RPS
parseRPS = choice [aWord "Rock" *> pure Rock,aWord "Paper" *> pure Paper,aWord "Scissors" *> pure Scissors]
  where
    aWord :: String -> Parser a
    aWord (c:cs) = (char (toLower c) <|> char (toUpper c)) *> aWord cs

beats :: RPS -> RPS -> Maybe Bool
Paper `beats` Rock = Just True
Rock `beats` Scissors = Just True
Scissors `beats` Paper = Just True
a `beats` b = if a == b then Nothing else not <$> (b `beats` a)

doGame :: IO ()
doGame = do
  putStrLn "Rock Paper Scissors! Enter your move below (r,p,s)"
  one <- prompt "Player One's move: "
  two <- prompt "Player Two's move: "
  case one `beats` two of
    Just True -> putStrLn "Player One wins!"
    Just False -> putStrLn "Player Two wins!"
    Nothing -> putStrLn "Tie!" >> doGame
  where
    prompt s = do
      putStr s 
      r <- getLine
      case parse parseRPS "" r of
        Left _ -> prompt s
        Right rps -> return rps
