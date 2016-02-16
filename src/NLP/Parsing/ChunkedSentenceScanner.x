{
{-# LANGUAGE OverloadedStrings #-}
module NLP.Parsing.ChunkedSentenceScanner where

import qualified Data.Text as T
import Data.Text (Text)
import NLP.Types.General (Error)
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

lexChunkTag :: Lexeme -> Either Error Text
lexChunkTag (ChunkStart _ txt) = Right $ T.pack txt
lexChunkTag _                  = Left "Not a ChunkStart lexeme"

main = do
  putStr "Hi > "
  s <- getContents
  print (alexScanTokens s)
}
