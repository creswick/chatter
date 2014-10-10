{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | The internal implementation of critical types in terms of the
-- Brown corpus.
module NLP.Corpora.Brown
 ( Tag(..)
 , Chunk(..)
 )
where

import Data.Serialize (Serialize)
import qualified Data.Text as T
import Data.Text (Text)
import Text.Read (readEither)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

import GHC.Generics

import qualified NLP.Types.Tags as T
import NLP.Types.General

data Chunk = C_NP -- ^ Noun Phrase.
           | C_VP -- ^ Verb Phrase.
           | C_PP -- ^ Prepositional Phrase.
           | C_CL -- ^ Clause.
  deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)

instance Arbitrary Chunk where
  arbitrary = elements [minBound ..]

instance Serialize Chunk

instance Serialize Tag

instance T.Tag Tag where
  fromTag = showBrownTag

  parseTag txt = case parseBrownTag txt of
                   Left  _ -> Unk
                   Right t -> t

  -- | Constant tag for "unknown"
  tagUNK = Unk

  tagTerm = showBrownTag

instance Arbitrary Tag where
  arbitrary = elements [minBound ..]

parseBrownTag :: Text -> Either Error Tag
parseBrownTag "(" = Right Op_Paren
parseBrownTag ")" = Right Cl_Paren
parseBrownTag "*" = Right Negator
parseBrownTag "," = Right Comma
parseBrownTag "-" = Right Dash
parseBrownTag "." = Right Term
parseBrownTag ":" = Right Colon
parseBrownTag txt =
  let normalized = replaceAll tagTxtPatterns (T.toUpper txt)
  in toEitherErr (readEither $ T.unpack normalized)


-- | Order matters here: The patterns are replaced in reverse order
-- when generating tags, and in top-to-bottom when generating tags.
tagTxtPatterns :: [(Text, Text)]
tagTxtPatterns = [ ("-", "_")
                 , ("+", "_pl_")
                 , ("*", "star")
                 , ("$", "dollar")
                 ]

reversePatterns :: [(Text, Text)]
reversePatterns = map (\(x,y) -> (y,x)) tagTxtPatterns

showBrownTag :: Tag -> Text
showBrownTag Op_Paren = "("
showBrownTag Cl_Paren = ")"
showBrownTag Negator = "*"
showBrownTag Comma = ","
showBrownTag Dash = "-"
showBrownTag Term = "."
showBrownTag Colon = ":"
showBrownTag tag = replaceAll reversePatterns (T.pack $ show tag)

replaceAll :: [(Text, Text)] -> (Text -> Text)
replaceAll patterns = foldl (.) id (map (uncurry T.replace) patterns)

instance T.ChunkTag Chunk where
  fromChunk = T.pack . show
  parseChunk txt = toEitherErr $ readEither (T.unpack $ T.append "C_" txt)

data Tag = Op_Paren -- ^ (
         | Cl_Paren -- ^ )
         | Negator -- ^ *, not n't
         | Comma -- ^ ,
         | Dash -- ^ -
         | Term -- ^ . Sentence Terminator
         | Colon -- ^ :
         | ABL -- ^ determiner/pronoun, pre-qualifier e.g.; quite such
               -- rather
         | ABN -- ^ determiner/pronoun, pre-quantifier e.g.; all half
               -- many nary
         | ABX -- ^ determiner/pronoun, double conjunction or
               -- pre-quantifier both
         | AP -- ^ determiner/pronoun, post-determiner many other next
              -- more last former little several enough most least
              -- only very few fewer past same Last latter less single
              -- plenty 'nough lesser certain various manye
              -- next-to-last particular final previous present nuf
         | APdollar -- ^ determiner/pronoun, post-determiner, genitive
                    -- e.g.; other's
         | AP_pl_AP -- ^ determiner/pronoun, post-determiner,
                    -- hyphenated pair e.g.; many-much
         | AT -- ^ article e.g.; the an no a every th' ever' ye
         | BE -- ^ verb "to be", infinitive or imperative e.g.; be
         | BED -- ^ verb "to be", past tense, 2nd person singular or
               -- all persons plural e.g.; were
         | BEDstar -- ^ verb "to be", past tense, 2nd person singular
                   -- or all persons plural, negated e.g.; weren't
         | BEDZ -- ^ verb "to be", past tense, 1st and 3rd person
                -- singular e.g.; was
         | BEDZstar -- ^ verb "to be", past tense, 1st and 3rd person
                    -- singular, negated e.g.; wasn't
         | BEG -- ^ verb "to be", present participle or gerund e.g.;
               -- being
         | BEM -- ^ verb "to be", present tense, 1st person singular
               -- e.g.; am
         | BEMstar -- ^ verb "to be", present tense, 1st person
                   -- singular, negated e.g.; ain't
         | BEN -- ^ verb "to be", past participle e.g.; been
         | BER -- ^ verb "to be", present tense, 2nd person singular
               -- or all persons plural e.g.; are art
         | BERstar -- ^ verb "to be", present tense, 2nd person
                   -- singular or all persons plural, negated e.g.;
                   -- aren't ain't
         | BEZ -- ^ verb "to be", present tense, 3rd person singular
               -- e.g.; is
         | BEZstar -- ^ verb "to be", present tense, 3rd person
                   -- singular, negated e.g.; isn't ain't
         | CC -- ^ conjunction, coordinating e.g.; and or but plus &
              -- either neither nor yet 'n' and/or minus an'
         | CD -- ^ numeral, cardinal e.g.; two one 1 four 2 1913 71 74
              -- 637 1937 8 five three million 87-31 29-5 seven 1,119
              -- fifty-three 7.5 billion hundred 125,000 1,700 60 100
              -- six ...
         | CDdollar -- ^ numeral, cardinal, genitive e.g.; 1960's
                    -- 1961's .404's
         | CS -- ^ conjunction, subordinating e.g.; that as after
              -- whether before while like because if since for than
              -- altho until so unless though providing once lest
              -- s'posin' till whereas whereupon supposing tho' albeit
              -- then so's 'fore
         | DO -- ^ verb "to do", uninflected present tense, infinitive
              -- or imperative e.g.; do dost
         | DOstar -- ^ verb "to do", uninflected present tense or
                  -- imperative, negated e.g.; don't
         | DO_pl_PPSS -- ^ verb "to do", past or present tense +
                      -- pronoun, personal, nominative, not 3rd person
                      -- singular e.g.; d'you
         | DOD -- ^ verb "to do", past tense e.g.; did done
         | DODstar -- ^ verb "to do", past tense, negated e.g.; didn't
         | DOZ -- ^ verb "to do", present tense, 3rd person singular
               -- e.g.; does
         | DOZstar -- ^ verb "to do", present tense, 3rd person
                   -- singular, negated e.g.; doesn't don't
         | DT -- ^ determiner/pronoun, singular e.g.; this each
              -- another that 'nother
         | DTdollar -- ^ determiner/pronoun, singular, genitive e.g.;
                    -- another's
         | DT_pl_BEZ -- ^ determiner/pronoun + verb "to be", present
                     -- tense, 3rd person singular e.g.; that's
         | DT_pl_MD -- ^ determiner/pronoun + modal auxillary e.g.;
                    -- that'll this'll
         | DTI -- ^ determiner/pronoun, singular or plural e.g.; any
               -- some
         | DTS -- ^ determiner/pronoun, plural e.g.; these those them
         | DTS_pl_BEZ -- ^ pronoun, plural + verb "to be", present
                      -- tense, 3rd person singular e.g.; them's
         | DTX -- ^ determiner, pronoun or double conjunction e.g.;
               -- neither either one
         | EX -- ^ existential there e.g.; there
         | EX_pl_BEZ -- ^ existential there + verb "to be", present
                     -- tense, 3rd person singular e.g.; there's
         | EX_pl_HVD -- ^ existential there + verb "to have", past
                     -- tense e.g.; there'd
         | EX_pl_HVZ -- ^ existential there + verb "to have", present
                     -- tense, 3rd person singular e.g.; there's
         | EX_pl_MD -- ^ existential there + modal auxillary e.g.;
                    -- there'll there'd
         | FW_star -- ^ foreign word: negator e.g.; pas non ne
         | FW_AT -- ^ foreign word: article e.g.; la le el un die der
                 -- ein keine eine das las les Il
         | FW_AT_pl_NN -- ^ foreign word: article + noun, singular,
                       -- common e.g.; l'orchestre l'identite l'arcade
                       -- l'ange l'assistance l'activite L'Universite
                       -- l'independance L'Union L'Unita l'osservatore
         | FW_AT_pl_NP -- ^ foreign word: article + noun, singular,
                       -- proper e.g.; L'Astree L'Imperiale
         | FW_BE -- ^ foreign word: verb "to be", infinitive or
                 -- imperative e.g.; sit
         | FW_BER -- ^ foreign word: verb "to be", present tense, 2nd
                  -- person singular or all persons plural e.g.; sind
                  -- sunt etes
         | FW_BEZ -- ^ foreign word: verb "to be", present tense, 3rd
                  -- person singular e.g.; ist est
         | FW_CC -- ^ foreign word: conjunction, coordinating e.g.; et
                 -- ma mais und aber och nec y
         | FW_CD -- ^ foreign word: numeral, cardinal e.g.; une cinq
                 -- deux sieben unam zwei
         | FW_CS -- ^ foreign word: conjunction, subordinating e.g.;
                 -- bevor quam ma
         | FW_DT -- ^ foreign word: determiner/pronoun, singular e.g.;
                 -- hoc
         | FW_DT_pl_BEZ -- ^ foreign word: determiner + verb "to be",
                        -- present tense, 3rd person singular e.g.;
                        -- c'est
         | FW_DTS -- ^ foreign word: determiner/pronoun, plural e.g.;
                  -- haec
         | FW_HV -- ^ foreign word: verb "to have", present tense, not
                 -- 3rd person singular e.g.; habe
         | FW_IN -- ^ foreign word: preposition e.g.; ad de en a par
                 -- con dans ex von auf super post sine sur sub avec
                 -- per inter sans pour pendant in di
         | FW_IN_pl_AT -- ^ foreign word: preposition + article e.g.;
                       -- della des du aux zur d'un del dell'
         | FW_IN_pl_NN -- ^ foreign word: preposition + noun,
                       -- singular, common e.g.; d'etat d'hotel
                       -- d'argent d'identite d'art
         | FW_IN_pl_NP -- ^ foreign word: preposition + noun,
                       -- singular, proper e.g.; d'Yquem d'Eiffel
         | FW_JJ -- ^ foreign word: adjective e.g.; avant Espagnol
                 -- sinfonica Siciliana Philharmonique grand publique
                 -- haute noire bouffe Douce meme humaine bel
                 -- serieuses royaux anticus presto Sovietskaya
                 -- Bayerische comique schwarzen ...
         | FW_JJR -- ^ foreign word: adjective, comparative e.g.;
                  -- fortiori
         | FW_JJT -- ^ foreign word: adjective, superlative e.g.;
                  -- optimo
         | FW_NN -- ^ foreign word: noun, singular, common e.g.;
                 -- ballet esprit ersatz mano chatte goutte sang
                 -- Fledermaus oud def kolkhoz roi troika canto boite
                 -- blutwurst carne muzyka bonheur monde piece force
                 -- ...
         | FW_NNdollar -- ^ foreign word: noun, singular, common,
                       -- genitive e.g.; corporis intellectus arte's
                       -- dei aeternitatis senioritatis curiae
                       -- patronne's chambre's
         | FW_NNS -- ^ foreign word: noun, plural, common e.g.; al
                  -- culpas vopos boites haflis kolkhozes augen
                  -- tyrannis alpha-beta-gammas metis banditos rata
                  -- phis negociants crus Einsatzkommandos kamikaze
                  -- wohaws sabinas zorrillas palazzi engages coureurs
                  -- corroborees yori Ubermenschen ...
         | FW_NP -- ^ foreign word: noun, singular, proper e.g.;
                 -- Karshilama Dieu Rundfunk Afrique Espanol Afrika
                 -- Spagna Gott Carthago deus
         | FW_NPS -- ^ foreign word: noun, plural, proper e.g.;
                  -- Svenskarna Atlantes Dieux
         | FW_NR -- ^ foreign word: noun, singular, adverbial e.g.;
                 -- heute morgen aujourd'hui hoy
         | FW_OD -- ^ foreign word: numeral, ordinal e.g.; 18e 17e
                 -- quintus
         | FW_PN -- ^ foreign word: pronoun, nominal e.g.; hoc
         | FW_PPdollar -- ^ foreign word: determiner, possessive e.g.;
                       -- mea mon deras vos
         | FW_PPL -- ^ foreign word: pronoun, singular, reflexive
                  -- e.g.; se
         | FW_PPL_pl_VBZ -- ^ foreign word: pronoun, singular,
                         -- reflexive + verb, present tense, 3rd
                         -- person singular e.g.; s'excuse s'accuse
         | FW_PPO -- ^ pronoun, personal, accusative e.g.; lui me moi
                  -- mi
         | FW_PPO_pl_IN -- ^ foreign word: pronoun, personal,
                        -- accusative + preposition e.g.; mecum tecum
         | FW_PPS -- ^ foreign word: pronoun, personal, nominative,
                  -- 3rd person singular e.g.; il
         | FW_PPSS -- ^ foreign word: pronoun, personal, nominative,
                   -- not 3rd person singular e.g.; ich vous sie je
         | FW_PPSS_pl_HV -- ^ foreign word: pronoun, personal,
                         -- nominative, not 3rd person singular + verb
                         -- "to have", present tense, not 3rd person
                         -- singular e.g.; j'ai
         | FW_QL -- ^ foreign word: qualifier e.g.; minus
         | FW_RB -- ^ foreign word: adverb e.g.; bas assai deja um
                 -- wiederum cito velociter vielleicht simpliciter non
                 -- zu domi nuper sic forsan olim oui semper tout
                 -- despues hors
         | FW_RB_pl_CC -- ^ foreign word: adverb + conjunction,
                       -- coordinating e.g.; forisque
         | FW_TO_pl_VB -- ^ foreign word: infinitival to + verb,
                       -- infinitive e.g.; d'entretenir
         | FW_UH -- ^ foreign word: interjection e.g.; sayonara bien
                 -- adieu arigato bonjour adios bueno tchalo ciao o
         | FW_VB -- ^ foreign word: verb, present tense, not 3rd
                 -- person singular, imperative or infinitive e.g.;
                 -- nolo contendere vive fermate faciunt esse vade
                 -- noli tangere dites duces meminisse iuvabit
                 -- gosaimasu voulez habla ksu'u'peli'afo lacheln
                 -- miuchi say allons strafe portant
         | FW_VBD -- ^ foreign word: verb, past tense e.g.; stabat
                  -- peccavi audivi
         | FW_VBG -- ^ foreign word: verb, present participle or
                  -- gerund e.g.; nolens volens appellant
                  -- seq. obliterans servanda dicendi delenda
         | FW_VBN -- ^ foreign word: verb, past participle e.g.; vue
                  -- verstrichen rasa verboten engages
         | FW_VBZ -- ^ foreign word: verb, present tense, 3rd person
                  -- singular e.g.; gouverne sinkt sigue diapiace
         | FW_WDT -- ^ foreign word: WH-determiner e.g.; quo qua quod
                  -- que quok
         | FW_WPO -- ^ foreign word: WH-pronoun, accusative e.g.;
                  -- quibusdam
         | FW_WPS -- ^ foreign word: WH-pronoun, nominative e.g.; qui
         | HV -- ^ verb "to have", uninflected present tense,
              -- infinitive or imperative e.g.; have hast
         | HVstar -- ^ verb "to have", uninflected present tense or
                  -- imperative, negated e.g.; haven't ain't
         | HV_pl_TO -- ^ verb "to have", uninflected present tense +
                    -- infinitival to e.g.; hafta
         | HVD -- ^ verb "to have", past tense e.g.; had
         | HVDstar -- ^ verb "to have", past tense, negated e.g.;
                   -- hadn't
         | HVG -- ^ verb "to have", present participle or gerund e.g.;
               -- having
         | HVN -- ^ verb "to have", past participle e.g.; had
         | HVZ -- ^ verb "to have", present tense, 3rd person singular
               -- e.g.; has hath
         | HVZstar -- ^ verb "to have", present tense, 3rd person
                   -- singular, negated e.g.; hasn't ain't
         | IN -- ^ preposition e.g.; of in for by considering to on
              -- among at through with under into regarding than since
              -- despite according per before toward against as after
              -- during including between without except upon out over
              -- ...
         | IN_pl_IN -- ^ preposition, hyphenated pair e.g.; f'ovuh
         | IN_pl_PPO -- ^ preposition + pronoun, personal, accusative
                     -- e.g.; t'hi-im
         | JJ -- ^ adjective e.g.; recent over-all possible
              -- hard-fought favorable hard meager fit such widespread
              -- outmoded inadequate ambiguous grand clerical
              -- effective orderly federal foster general
              -- proportionate ...
         | JJdollar -- ^ adjective, genitive e.g.; Great's
         | JJ_pl_JJ -- ^ adjective, hyphenated pair e.g.; big-large
                    -- long-far
         | JJR -- ^ adjective, comparative e.g.; greater older further
               -- earlier later freer franker wider better deeper
               -- firmer tougher faster higher bigger worse younger
               -- lighter nicer slower happier frothier Greater newer
               -- Elder ...
         | JJR_pl_CS -- ^ adjective + conjunction, coordinating e.g.;
                     -- lighter'n
         | JJS -- ^ adjective, semantically superlative e.g.; top
               -- chief principal northernmost master key head main
               -- tops utmost innermost foremost uppermost paramount
               -- topmost
         | JJT -- ^ adjective, superlative e.g.; best largest coolest
               -- calmest latest greatest earliest simplest strongest
               -- newest fiercest unhappiest worst youngest worthiest
               -- fastest hottest fittest lowest finest smallest
               -- staunchest ...
         | MD -- ^ modal auxillary e.g.; should may might will would
              -- must can could shall ought need wilt
         | MDstar -- ^ modal auxillary, negated e.g.; cannot couldn't
                  -- wouldn't can't won't shouldn't shan't mustn't
                  -- musn't
         | MD_pl_HV -- ^ modal auxillary + verb "to have", uninflected
                    -- form e.g.; shouldda musta coulda must've woulda
                    -- could've
         | MD_pl_PPSS -- ^ modal auxillary + pronoun, personal,
                      -- nominative, not 3rd person singular e.g.;
                      -- willya
         | MD_pl_TO -- ^ modal auxillary + infinitival to e.g.; oughta
         | NN -- ^ noun, singular, common e.g.; failure burden court
              -- fire appointment awarding compensation Mayor interim
              -- committee fact effect airport management surveillance
              -- jail doctor intern extern night weekend duty
              -- legislation Tax Office ...
         | NNdollar -- ^ noun, singular, common, genitive e.g.;
                    -- season's world's player's night's chapter's
                    -- golf's football's baseball's club's U.'s
                    -- coach's bride's bridegroom's board's county's
                    -- firm's company's superintendent's mob's Navy's
                    -- ...
         | NN_pl_BEZ -- ^ noun, singular, common + verb "to be",
                     -- present tense, 3rd person singular e.g.;
                     -- water's camera's sky's kid's Pa's heat's
                     -- throat's father's money's undersecretary's
                     -- granite's level's wife's fat's Knife's fire's
                     -- name's hell's leg's sun's roulette's cane's
                     -- guy's kind's baseball's ...
         | NN_pl_HVD -- ^ noun, singular, common + verb "to have",
                     -- past tense e.g.; Pa'd
         | NN_pl_HVZ -- ^ noun, singular, common + verb "to have",
                     -- present tense, 3rd person singular e.g.; guy's
                     -- Knife's boat's summer's rain's company's
         | NN_pl_IN -- ^ noun, singular, common + preposition e.g.;
                    -- buncha
         | NN_pl_MD -- ^ noun, singular, common + modal auxillary
                    -- e.g.; cowhand'd sun'll
         | NN_pl_NN -- ^ noun, singular, common, hyphenated pair e.g.;
                    -- stomach-belly
         | NNS -- ^ noun, plural, common e.g.; irregularities
               -- presentments thanks reports voters laws legislators
               -- years areas adjustments chambers $100 bonds courts
               -- sales details raises sessions members congressmen
               -- votes polls calls ...
         | NNSdollar -- ^ noun, plural, common, genitive e.g.;
                     -- taxpayers' children's members' States' women's
                     -- cutters' motorists' steelmakers' hours'
                     -- Nations' lawyers' prisoners' architects'
                     -- tourists' Employers' secretaries' Rogues' ...
         | NNS_pl_MD -- ^ noun, plural, common + modal auxillary e.g.;
                     -- duds'd oystchers'll
         | NP -- ^ noun, singular, proper e.g.; Fulton Atlanta
              -- September-October Durwood Pye Ivan Allen
              -- Jr. Jan. Alpharetta Grady William B. Hartsfield Pearl
              -- Williams Aug. Berry J. M. Cheshire Griffin Opelika
              -- Ala. E. Pelham Snodgrass ...
         | NPdollar -- ^ noun, singular, proper, genitive e.g.;
                    -- Green's Landis' Smith's Carreon's Allison's
                    -- Boston's Spahn's Willie's Mickey's Milwaukee's
                    -- Mays' Howsam's Mantle's Shaw's Wagner's
                    -- Rickey's Shea's Palmer's Arnold's Broglio's ...
         | NP_pl_BEZ -- ^ noun, singular, proper + verb "to be",
                     -- present tense, 3rd person singular e.g.; W.'s
                     -- Ike's Mack's Jack's Kate's Katharine's Black's
                     -- Arthur's Seaton's Buckhorn's Breed's Penny's
                     -- Rob's Kitty's Blackwell's Myra's Wally's
                     -- Lucille's Springfield's Arlene's
         | NP_pl_HVZ -- ^ noun, singular, proper + verb "to have",
                     -- present tense, 3rd person singular e.g.;
                     -- Bill's Guardino's Celie's Skolman's Crosson's
                     -- Tim's Wally's
         | NP_pl_MD -- ^ noun, singular, proper + modal auxillary
                    -- e.g.; Gyp'll John'll
         | NPS -- ^ noun, plural, proper e.g.; Chases Aderholds
               -- Chapelles Armisteads Lockies Carbones French
               -- Marskmen Toppers Franciscans Romans Cadillacs Masons
               -- Blacks Catholics British Dixiecrats Mississippians
               -- Congresses ...
         | NPSdollar -- ^ noun, plural, proper, genitive e.g.;
                     -- Republicans' Orioles' Birds' Yanks' Redbirds'
                     -- Bucs' Yankees' Stevenses' Geraghtys' Burkes'
                     -- Wackers' Achaeans' Dresbachs' Russians'
                     -- Democrats' Gershwins' Adventists' Negroes'
                     -- Catholics' ...
         | NR -- ^ noun, singular, adverbial e.g.; Friday home
              -- Wednesday Tuesday Monday Sunday Thursday yesterday
              -- tomorrow tonight West East Saturday west left east
              -- downtown north northeast southeast northwest North
              -- South right ...
         | NRdollar -- ^ noun, singular, adverbial, genitive e.g.;
                    -- Saturday's Monday's yesterday's tonight's
                    -- tomorrow's Sunday's Wednesday's Friday's
                    -- today's Tuesday's West's Today's South's
         | NR_pl_MD -- ^ noun, singular, adverbial + modal auxillary
                    -- e.g.; today'll
         | NRS -- ^ noun, plural, adverbial e.g.; Sundays Mondays
               -- Saturdays Wednesdays Souths Fridays
         | OD -- ^ numeral, ordinal e.g.; first 13th third nineteenth
              -- 2d 61st second sixth eighth ninth twenty-first
              -- eleventh 50th eighteenth- Thirty-ninth 72nd 1/20th
              -- twentieth mid-19th thousandth 350th sixteenth 701st
              -- ...
         | PN -- ^ pronoun, nominal e.g.; none something everything
              -- one anyone nothing nobody everybody everyone anybody
              -- anything someone no-one nothin
         | PNdollar -- ^ pronoun, nominal, genitive e.g.; one's
                    -- someone's anybody's nobody's everybody's
                    -- anyone's everyone's
         | PN_pl_BEZ -- ^ pronoun, nominal + verb "to be", present
                     -- tense, 3rd person singular e.g.; nothing's
                     -- everything's somebody's nobody's someone's
         | PN_pl_HVD -- ^ pronoun, nominal + verb "to have", past
                     -- tense e.g.; nobody'd
         | PN_pl_HVZ -- ^ pronoun, nominal + verb "to have", present
                     -- tense, 3rd person singular e.g.; nobody's
                     -- somebody's one's
         | PN_pl_MD -- ^ pronoun, nominal + modal auxillary e.g.;
                    -- someone'll somebody'll anybody'd
         | PPdollar -- ^ determiner, possessive e.g.; our its his
                    -- their my your her out thy mine thine
         | PPdollardollar -- ^ pronoun, possessive e.g.; ours mine his
                          -- hers theirs yours
         | PPL -- ^ pronoun, singular, reflexive e.g.; itself himself
               -- myself yourself herself oneself ownself
         | PPLS -- ^ pronoun, plural, reflexive e.g.; themselves
                -- ourselves yourselves
         | PPO -- ^ pronoun, personal, accusative e.g.; them it him me
               -- us you 'em her thee we'uns
         | PPS -- ^ pronoun, personal, nominative, 3rd person singular
               -- e.g.; it he she thee
         | PPS_pl_BEZ -- ^ pronoun, personal, nominative, 3rd person
                      -- singular + verb "to be", present tense, 3rd
                      -- person singular e.g.; it's he's she's
         | PPS_pl_HVD -- ^ pronoun, personal, nominative, 3rd person
                      -- singular + verb "to have", past tense e.g.;
                      -- she'd he'd it'd
         | PPS_pl_HVZ -- ^ pronoun, personal, nominative, 3rd person
                      -- singular + verb "to have", present tense, 3rd
                      -- person singular e.g.; it's he's she's
         | PPS_pl_MD -- ^ pronoun, personal, nominative, 3rd person
                     -- singular + modal auxillary e.g.; he'll she'll
                     -- it'll he'd it'd she'd
         | PPSS -- ^ pronoun, personal, nominative, not 3rd person
                -- singular e.g.; they we I you ye thou you'uns
         | PPSS_pl_BEM -- ^ pronoun, personal, nominative, not 3rd
                       -- person singular + verb "to be", present
                       -- tense, 1st person singular e.g.; I'm Ahm
         | PPSS_pl_BER -- ^ pronoun, personal, nominative, not 3rd
                       -- person singular + verb "to be", present
                       -- tense, 2nd person singular or all persons
                       -- plural e.g.; we're you're they're
         | PPSS_pl_BEZ -- ^ pronoun, personal, nominative, not 3rd
                       -- person singular + verb "to be", present
                       -- tense, 3rd person singular e.g.; you's
         | PPSS_pl_BEZstar -- ^ pronoun, personal, nominative, not 3rd
                           -- person singular + verb "to be", present
                           -- tense, 3rd person singular, negated
                           -- e.g.; 'tain't
         | PPSS_pl_HV -- ^ pronoun, personal, nominative, not 3rd
                      -- person singular + verb "to have", uninflected
                      -- present tense e.g.; I've we've they've you've
         | PPSS_pl_HVD -- ^ pronoun, personal, nominative, not 3rd
                       -- person singular + verb "to have", past tense
                       -- e.g.; I'd you'd we'd they'd
         | PPSS_pl_MD -- ^ pronoun, personal, nominative, not 3rd
                      -- person singular + modal auxillary e.g.;
                      -- you'll we'll I'll we'd I'd they'll they'd
                      -- you'd
         | PPSS_pl_VB -- ^ pronoun, personal, nominative, not 3rd
                      -- person singular + verb "to verb", uninflected
                      -- present tense e.g.; y'know
         | QL -- ^ qualifier, pre e.g.; well less very most so real as
              -- highly fundamentally even how much remarkably
              -- somewhat more completely too thus ill deeply little
              -- overly halfway almost impossibly far severly such ...
         | QLP -- ^ qualifier, post e.g.; indeed enough still 'nuff
         | RB -- ^ adverb e.g.; only often generally also nevertheless
              -- upon together back newly no likely meanwhile near
              -- then heavily there apparently yet outright fully
              -- aside consistently specifically formally ever just
              -- ...
         | RBdollar -- ^ adverb, genitive e.g.; else's
         | RB_pl_BEZ -- ^ adverb + verb "to be", present tense, 3rd
                     -- person singular e.g.; here's there's
         | RB_pl_CS -- ^ adverb + conjunction, coordinating e.g.;
                    -- well's soon's
         | RBR -- ^ adverb, comparative e.g.; further earlier better
               -- later higher tougher more harder longer sooner less
               -- faster easier louder farther oftener nearer cheaper
               -- slower tighter lower worse heavier quicker ...
         | RBR_pl_CS -- ^ adverb, comparative + conjunction,
                     -- coordinating e.g.; more'n
         | RBT -- ^ adverb, superlative e.g.; most best highest
               -- uppermost nearest brightest hardest fastest deepest
               -- farthest loudest ...
         | RN -- ^ adverb, nominal e.g.; here afar then
         | RP -- ^ adverb, particle e.g.; up out off down over on in
              -- about through across after
         | RP_pl_IN -- ^ adverb, particle + preposition e.g.; out'n
                    -- outta
         | TO -- ^ infinitival to e.g.; to t'
         | TO_pl_VB -- ^ infinitival to + verb, infinitive e.g.;
                    -- t'jawn t'lah
         | UH -- ^ interjection e.g.; Hurrah bang whee hmpf ah goodbye
              -- oops oh-the-pain-of-it ha crunch say oh why see well
              -- hello lo alas tarantara rum-tum-tum gosh hell keerist
              -- Jesus Keeeerist boy c'mon 'mon goddamn bah hoo-pig
              -- damn ...
         | VB -- ^ verb, base: uninflected present, imperative or
              -- infinitive e.g.; investigate find act follow inure
              -- achieve reduce take remedy re-set distribute realize
              -- disable feel receive continue place protect eliminate
              -- elaborate work permit run enter force ...
         | VB_pl_AT -- ^ verb, base: uninflected present or infinitive
                    -- + article e.g.; wanna
         | VB_pl_IN -- ^ verb, base: uninflected present, imperative
                    -- or infinitive + preposition e.g.; lookit
         | VB_pl_JJ -- ^ verb, base: uninflected present, imperative
                    -- or infinitive + adjective e.g.; die-dead
         | VB_pl_PPO -- ^ verb, uninflected present tense + pronoun,
                     -- personal, accusative e.g.; let's lemme gimme
         | VB_pl_RP -- ^ verb, imperative + adverbial particle e.g.;
                    -- g'ahn c'mon
         | VB_pl_TO -- ^ verb, base: uninflected present, imperative
                    -- or infinitive + infinitival to e.g.; wanta
                    -- wanna
         | VB_pl_VB -- ^ verb, base: uninflected present, imperative
                    -- or infinitive; hypenated pair e.g.; say-speak
         | VBD -- ^ verb, past tense e.g.; said produced took
               -- recommended commented urged found added praised
               -- charged listed became announced brought attended
               -- wanted voted defeated received got stood shot
               -- scheduled feared promised made ...
         | VBG -- ^ verb, present participle or gerund e.g.;
               -- modernizing improving purchasing Purchasing lacking
               -- enabling pricing keeping getting picking entering
               -- voting warning making strengthening setting
               -- neighboring attending participating moving ...
         | VBG_pl_TO -- ^ verb, present participle + infinitival to
                     -- e.g.; gonna
         | VBN -- ^ verb, past participle e.g.; conducted charged won
               -- received studied revised operated accepted combined
               -- experienced recommended effected granted seen
               -- protected adopted retarded notarized selected
               -- composed gotten printed ...
         | VBN_pl_TO -- ^ verb, past participle + infinitival to e.g.;
                     -- gotta
         | VBZ -- ^ verb, present tense, 3rd person singular e.g.;
               -- deserves believes receives takes goes expires says
               -- opposes starts permits expects thinks faces votes
               -- teaches holds calls fears spends collects backs
               -- eliminates sets flies gives seeks reads ...
         | WDT -- ^ WH-determiner e.g.; which what whatever whichever
               -- whichever-the-hell
         | WDT_pl_BER -- ^ WH-determiner + verb "to be", present
                      -- tense, 2nd person singular or all persons
                      -- plural e.g.; what're
         | WDT_pl_BER_pl_PP -- ^ WH-determiner + verb "to be",
                            -- present, 2nd person singular or all
                            -- persons plural + pronoun, personal,
                            -- nominative, not 3rd person singular
                            -- e.g.; whaddya
         | WDT_pl_BEZ -- ^ WH-determiner + verb "to be", present
                      -- tense, 3rd person singular e.g.; what's
         | WDT_pl_DO_pl_PPS -- ^ WH-determiner + verb "to do",
                            -- uninflected present tense + pronoun,
                            -- personal, nominative, not 3rd person
                            -- singular e.g.; whaddya
         | WDT_pl_DOD -- ^ WH-determiner + verb "to do", past tense
                      -- e.g.; what'd
         | WDT_pl_HVZ -- ^ WH-determiner + verb "to have", present
                      -- tense, 3rd person singular e.g.; what's
         | WPdollar -- ^ WH-pronoun, genitive e.g.; whose whosever
         | WPO -- ^ WH-pronoun, accusative e.g.; whom that who
         | WPS -- ^ WH-pronoun, nominative e.g.; that who whoever
               -- whosoever what whatsoever
         | WPS_pl_BEZ -- ^ WH-pronoun, nominative + verb "to be",
                      -- present, 3rd person singular e.g.; that's
                      -- who's
         | WPS_pl_HVD -- ^ WH-pronoun, nominative + verb "to have",
                      -- past tense e.g.; who'd
         | WPS_pl_HVZ -- ^ WH-pronoun, nominative + verb "to have",
                      -- present tense, 3rd person singular e.g.;
                      -- who's that's
         | WPS_pl_MD -- ^ WH-pronoun, nominative + modal auxillary
                     -- e.g.; who'll that'd who'd that'll
         | WQL -- ^ WH-qualifier e.g.; however how
         | WRB -- ^ WH-adverb e.g.; however when where why whereby
               -- wherever how whenever whereon wherein wherewith
               -- wheare wherefore whereof howsabout
         | WRB_pl_BER -- ^ WH-adverb + verb "to be", present, 2nd
                      -- person singular or all persons plural e.g.;
                      -- where're
         | WRB_pl_BEZ -- ^ WH-adverb + verb "to be", present, 3rd
                      -- person singular e.g.; how's where's
         | WRB_pl_DO -- ^ WH-adverb + verb "to do", present, not 3rd
                     -- person singular e.g.; howda
         | WRB_pl_DOD -- ^ WH-adverb + verb "to do", past tense e.g.;
                      -- where'd how'd
         | WRB_pl_DODstar -- ^ WH-adverb + verb "to do", past tense,
                          -- negated e.g.; whyn't
         | WRB_pl_DOZ -- ^ WH-adverb + verb "to do", present tense,
                      -- 3rd person singular e.g.; how's
         | WRB_pl_IN -- ^ WH-adverb + preposition e.g.; why'n
         | WRB_pl_MD -- ^ WH-adverb + modal auxillary e.g.; where'd
         | Unk       -- ^ Unknown.
  deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)
