{
module NLP.Parsing.ChunkedSentenceScanner where
}

%wrapper "posn"

-- $alpha = [a-zA-Z0-9!@#\$\%\^\&\*\(\)\}\{-`~'\,\.\"\<\>\?\+\|\;\:]		-- alphabetic characters
$alpha = [a-zA-Z0-9\!\@\#\$\%\^\&\*\(\)\}\{\-\`\~\'\,\.\"\<\>\?\+\|\;\:]		-- alphabetic characters

tokens :-

  $white+		  ;
  \[$alpha+   { \(AlexPn x _ _) s -> (x, ChunkStart $ tail s) }
  \]          { \(AlexPn x _ _) s -> (x, ChunkEnd) }
  \/$alpha+   { \(AlexPn x _ _) s -> (x, Pos $ tail s) }
  $alpha+     { \(AlexPn x _ _) s -> (x, Tok s) }

{
-- Each action has type :: String -> Token

-- The token type:
data Lexeme = ChunkStart String
            | ChunkEnd
            | Pos String
            | Tok String
	deriving (Eq,Show)

main = do
  putStr "Hi > "
  s <- getContents
  print (alexScanTokens s)
}
