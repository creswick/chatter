{
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.Parsing.ChunkedSentenceParser where

import NLP.Parsing.ChunkedSentenceScanner
import NLP.Types.General (Error)

import           Data.Either (rights)
import qualified Data.Text as T

}


%name parse
%tokentype { Lexeme }
%error { parseError }

%token
  '[' { ChunkStart _ _ }
  ']' { ChunkEnd _ }
  pos { Pos _ _ }
  tok { Tok _ _ }

%%

CS           :: { CS }
             : CS ChunkOrChink { appendCS $1 $2 }
             | ChunkOrChink    { CS [ $1 ]      }

PosTok       :: { PosTok }
             : tok pos         { PosTok $1 $2 (lexPos $1)  }

PosToks      :: { PosToks }
             : PosToks PosTok  { appendPosToks $1 $2 }
             | PosTok          { PosToks [ $1 ] }

ChunkOrChink :: { ChunkOrChink }
ChunkOrChink : '[' PosToks ']' { Chunk $1 $2 }
             | PosToks         { Chink $1    }

{

parseError :: [Lexeme] -> a
parseError _ = error "Parse error"

data PosTok = PosTok { ptTok :: Lexeme
                     , ptPos :: Lexeme
                     , ptLoc :: Int
                     } deriving Show

ptPosText :: PosTok -> Either Error T.Text
ptPosText PosTok {..} = case ptPos of
                          (Pos _ txt) -> Right (T.pack txt)
                          _           -> Left "Not a POS lexeme"

ptTokText :: PosTok -> Either Error T.Text
ptTokText PosTok {..} = case ptTok of
                          (Tok _ txt) -> Right (T.pack txt)
                          _           -> Left "Not a Tok lexeme"

data PosToks = PosToks { ptsToks :: [PosTok]
                       } deriving Show

appendPosToks :: PosToks -> PosTok -> PosToks
appendPosToks (PosToks ts) t = PosToks (ts++[t])

data ChunkOrChink = Chunk { cocStr :: Lexeme
                          , cocPTS :: PosToks
                          }
                  | Chink { cocPTS :: PosToks
                          }
                   deriving Show

-- | Count the number of PosToks in a `ChunkOrChink`
tokLength :: ChunkOrChink -> Int
tokLength coc = length $ ptsToks $ cocPTS coc

data CS = CS { csCOCs :: [ChunkOrChink]
             } deriving Show

appendCS :: CS -> ChunkOrChink -> CS
appendCS (CS cs) coc = CS (cs ++ [coc])

getPOSToks :: CS -> [PosTok]
getPOSToks cs = concatMap (ptsToks . cocPTS) (csCOCs cs)

-- | Get the underlying text tokens, devoid of the tagging / chunking markup.
getNaturalText :: CS -> [T.Text]
getNaturalText cs = rights $ map ptTokText $ getPOSToks cs

testParser = print . parse . alexScanTokens
}
