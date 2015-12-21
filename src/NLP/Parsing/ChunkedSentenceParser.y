{
module NLP.Parsing.ChunkedSentenceParser where

import NLP.Parsing.ChunkedSentenceScanner
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
             : CS ChunkOrChink { appendCS $1 $2            }
             | ChunkOrChink    { CS [ $1 ] (cocLoc $1)     }

PosTok       :: { PosTok }
             : tok pos         { PosTok $1 $2 (lexPos $1)  }

PosToks      :: { PosToks }
             : PosToks PosTok  { appendPosToks $1 $2 }
             | PosTok          { PosToks [ $1 ] (ptLoc $1) }

ChunkOrChink :: { ChunkOrChink }
ChunkOrChink : '[' PosToks ']' { Chunk $1 $2 (lexPos $1)   }
             | PosToks         { Chink $1 (ptsLoc $1)      }

{

parseError :: [Lexeme] -> a
parseError _ = error "Parse error"

data PosTok = PosTok { ptTok :: Lexeme
                     , ptPos :: Lexeme
                     , ptLoc :: Int
                     } deriving Show

data PosToks = PosToks { ptsToks :: [PosTok]
                       , ptsLoc :: Int
                       } deriving Show

appendPosToks :: PosToks -> PosTok -> PosToks
appendPosToks (PosToks ts x) t = PosToks (ts++[t]) x

data ChunkOrChink = Chunk { cocStr :: Lexeme
                          , cocPTS :: PosToks
                          , cocLoc :: Int
                          }
                  | Chink { cocPTS :: PosToks
                          , cocLoc :: Int
                          }
                   deriving Show

data CS = CS { csCOCs :: [ChunkOrChink]
             , csLoc :: Int
} deriving Show

appendCS :: CS -> ChunkOrChink -> CS
appendCS (CS cs x) coc = CS (cs ++ [coc]) x

testParser = print . parse . alexScanTokens
}
