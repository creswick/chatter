{
module NLP.Parsing.ChunkedSentenceScanner where
}

%wrapper "posn"

$alpha = [a-zA-Z0-9\!\@\#\$\%\^\&\*\(\)\}\{\-\`\~\'\,\.\"\<\>\?\+\|\;\:]		-- alphabetic characters

tokens :-

  $white+		  ;
  \[$alpha+   { \(AlexPn x _ _) s -> ChunkStart x $ tail s }
  \]          { \(AlexPn x _ _) s -> ChunkEnd x }
  \/$alpha+   { \(AlexPn x _ _) s -> Pos x $ tail s }
  $alpha+     { \(AlexPn x _ _) s -> Tok x s }

{
-- Each action has type :: String -> Token

-- The token type:
data Lexeme = ChunkStart Int String
            | ChunkEnd Int
            | Pos Int String
            | Tok Int String
	deriving (Eq,Show)

-- | Extract the absolute character offset from a 'Lexeme'.
lexPos :: Lexeme -> Int
lexPos (ChunkStart x _) = x
lexPos (ChunkEnd x)     = x
lexPos (Pos x _)        = x
lexPos (Tok x _)        = x

main = do
  putStr "Hi > "
  s <- getContents
  print (alexScanTokens s)
}
