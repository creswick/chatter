{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Exp

where


import Data.HList

data Key = Key Int
  deriving (Read, Show)
data Name = Name String
  deriving (Read, Show)
data Breed = Cow | Sheep | Bird
  deriving (Read, Show)
data Price = Price Double
  deriving (Read, Show)

-- type Animal = Key :~: Name :~: Breed :~: Price :~: HNil

type Animal = HList '[Key, Name, Breed, Price]

angus :: Animal
angus = Key 42
  .*. Name "Angus"
  .*. Cow
  .*. Price 75.5
  .*. HNil

mutant = Key 42
  .*. Name "Angus"
  .*. Cow
  .*. Price 75.5
  .*. Sheep
  .*. HNil

plant = Key 42
  .*. Name "Angus"
  .*. Price 75.5
  .*. HNil

angusBreeds :: [Breed]
angusBreeds = hOccursMany angus -- [Cow]

mutantBreeds :: [Breed]
mutantBreeds = hOccursMany mutant -- [Cow,Sheep]

plantBreeds :: [Breed]
plantBreeds = hOccursMany plant -- []

listything = [Key 42]
  .*. [Cow, Sheep]
  .*. [Price 0.5]
  .*. HNil

listythingBreeds :: [Breed]
listythingBreeds = hOccurs listything

data FootNMouth = FootNMouth -- a namespace
  deriving (Read, Show)

key = firstLabel FootNMouth "key"
name = nextLabel key "name"
breed = nextLabel name "breed"
price = nextLabel breed "price"

unpricedAngus = key .=. (42::Integer)
  .*. name .=. "Angus"
  .*. breed .=. Cow
  .*. emptyRecord




-- newtype ArcType = ArcType (HList '[[Node (Maybe Int)], [Node (Maybe String)]])


-- empty :: ArcType
empty = [].*.[].*.HNil

data Node payload  = Node { payload :: Maybe payload -- ^ Nothing for start node.
                          , outList :: HList '[[Node Int], [Node String]]
                          }
                   deriving (Read, Show)

world = Node (Just "world") ([Node (Just 5) empty].*.[].*.HNil)

graph = Node Nothing ( [ Node (Just 5)       ([].*.[world].*.HNil) ]
                   .*. [ Node (Just "hello") ([].*.[world].*.HNil)
                       , Node (Just "howdy") ([Node (Just 42) empty].*.[world].*.HNil)
                       ]
                   .*. HNil)

towords (Node Nothing next) = case (hOccurs next :: [Node String]) of
                                [] -> [""]
                                xs -> concatMap towords xs
towords (Node (Just str) next) = case (hOccurs next :: [Node String]) of
                                   [] -> [str]
                                   xs -> map (\sfx -> str ++ " " ++ sfx) $ concatMap towords xs


