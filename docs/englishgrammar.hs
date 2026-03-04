{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
module EnglishGrammarComplete where

import Data.Maybe (isNothing, isJust, catMaybes, maybeToList)

-- =========================================================
-- 1. Core Coordinates & Types
-- =========================================================

data Tense = Past | Present | Future deriving (Show, Eq)
data Person = First | Second | Third deriving (Show, Eq)
data Number = Singular | Plural deriving (Show, Eq)

data Voice = Active | Passive deriving (Show, Eq)
data Polarity = Affirmative | Negative deriving (Show, Eq)
data Style = Full | Contracted deriving (Show, Eq)

data Modality = Can | Should | Must | Would | May | Might | Will deriving (Show, Eq)

data VerbClass = Lexical | Copular deriving (Show, Eq)

data ClauseForm = TakesThat | TakesInf | TakesGerund deriving (Show, Eq)

data Transitivity
  = Intransitive
  | Transitive
  | Ditransitive
  | Predicative
  | SubcatClause ClauseForm
  deriving (Show, Eq)

data NP = NP
  { npText   :: String
  , npPerson :: Person
  , npNumber :: Number
  } deriving (Show, Eq)

data SubjectNP
  = RefNP NP
  | ExplIt
  | ExplThere
  | WhSubjNP String
  deriving (Show, Eq)

data Verb = Verb
  { vBase       :: String
  , v3sg        :: String
  , vPast       :: String
  , vPastPart   :: String
  , vPresPart   :: String
  , vClass      :: VerbClass
  , vTrans      :: Transitivity
  , vAllowsProg :: Bool
  } deriving (Show, Eq)

data Arguments = Arguments
  { argObj     :: Maybe NP
  , argIndObj  :: Maybe NP
  , argBy      :: Maybe NP
  , argPred    :: Maybe String
  , argClause  :: Maybe String
  } deriving (Show, Eq)

data ClauseType
  = Declarative
  | YesNoQuestion
  | WhObject String
  | WhAdjunct String
  | WhSubject
  | TagQuestion
  deriving (Show, Eq)

data FiniteSpec = FiniteSpec
  { fsTense       :: Tense
  , fsModal       :: Maybe Modality
  , fsPerfect     :: Bool
  , fsProgressive :: Bool
  , fsVoice       :: Voice
  , fsPolarity    :: Polarity
  , fsClauseType  :: ClauseType
  } deriving (Show, Eq)

data Complementizer = ThatC | IfC | WhetherC | BecauseC deriving (Show, Eq)
data RelPron = ThatRel | WhichRel | WhoRel deriving (Show, Eq)
data RelGap = RelSubj | RelObj deriving (Show, Eq)

data ImperativeSpec = ImperativeSpec
  { isPerfect     :: Bool
  , isProgressive :: Bool
  , isVoice       :: Voice
  , isPolarity    :: Polarity
  } deriving (Show, Eq)

data ParticipleForm
  = PresParticipleActive | PresParticiplePassive
  | PastParticiplePassive | PerfParticipleActive | PerfParticiplePassive
  deriving (Show, Eq)

data NonFinite
  = Infinitive Bool Bool Voice Polarity
  | Gerund Bool Voice Polarity
  | Participle ParticipleForm Polarity
  deriving (Show, Eq)

-- =========================================================
-- 2. Aux & Agreement Machinery
-- =========================================================

data Aux = AModal String | AHave | ABe | ADo deriving (Show, Eq)

agreeNP :: SubjectNP -> Arguments -> NP
agreeNP (RefNP np) _ = np
agreeNP ExplIt _ = NP "it" Third Singular
agreeNP ExplThere args = maybe (NP "there" Third Singular) id (argObj args)
agreeNP (WhSubjNP w) _ = NP w Third Singular

is3sg :: NP -> Bool
is3sg np = npPerson np == Third && npNumber np == Singular

modalWord :: Modality -> String
modalWord m = case m of
  Can -> "can"; Should -> "should"; Must -> "must"
  Would -> "would"; May -> "may"; Might -> "might"; Will -> "will"

finiteDo :: Tense -> NP -> String
finiteDo t np = case t of
  Past    -> "did"
  Present -> if is3sg np then "does" else "do"
  Future  -> "do"

finiteHave :: Tense -> NP -> String
finiteHave t np = case t of
  Past    -> "had"
  Present -> if is3sg np then "has" else "have"
  Future  -> "have"

finiteBe :: Tense -> NP -> String
finiteBe t np = case t of
  Present
    | npPerson np == First && npNumber np == Singular -> "am"
    | is3sg np                                        -> "is"
    | otherwise                                       -> "are"
  Past
    | npPerson np == Second || npNumber np == Plural  -> "were"
    | otherwise                                       -> "was"
  Future -> "be"

finiteLex :: Tense -> NP -> Verb -> String
finiteLex t np v = case t of
  Past    -> vPast v
  Present -> if is3sg np then v3sg v else vBase v
  Future  -> vBase v

allAuxFinite :: FiniteSpec -> [Aux]
allAuxFinite s =
  let m = case fsModal s of
            Just md -> Just (AModal (modalWord md))
            Nothing -> if fsTense s == Future then Just (AModal "will") else Nothing
      struct = (if fsPerfect s then [AHave] else [])
            ++ (if fsProgressive s then [ABe] else [])
            ++ (if fsVoice s == Passive then [ABe] else [])
  in maybeToList m ++ struct

realizeFirstAux :: Tense -> NP -> Aux -> String
realizeFirstAux _ _  (AModal w) = w
realizeFirstAux t np AHave      = finiteHave t np
realizeFirstAux t np ABe        = finiteBe t np
realizeFirstAux t np ADo        = finiteDo t np

linkForm :: Aux -> Aux -> String
linkForm (AModal _) AHave = "have"
linkForm (AModal _) ABe   = "be"
linkForm AHave ABe        = "been"
linkForm ABe ABe          = "being"
linkForm _ _              = error "Invalid aux transition"

mainForm :: Bool -> Bool -> Voice -> Verb -> String
mainForm perfect progressive voice v
  | voice == Passive = vPastPart v
  | progressive      = vPresPart v
  | perfect          = vPastPart v
  | otherwise        = vBase v

contract :: String -> String
contract w = case w of
  -- NOTE: "am" intentionally not contracted to "aren't" in declaratives.
  "is"     -> "isn't";  "are"   -> "aren't"
  "was"    -> "wasn't"; "were"  -> "weren't"
  "has"    -> "hasn't"; "have"  -> "haven't"; "had" -> "hadn't"
  "does"   -> "doesn't";"do"    -> "don't";   "did" -> "didn't"
  "can"    -> "can't";  "will"  -> "won't"
  "should" -> "shouldn't"; "would" -> "wouldn't"
  "must"   -> "mustn't"; "might" -> "mightn't"
  "may"    -> "may not"
  _        -> w ++ " not"

negateFirst :: Style -> String -> [String]
negateFirst Full a = [a, "not"]
negateFirst Contracted a =
  if a == "am" then ["am", "not"] else words (contract a)

isInvertingClause :: ClauseType -> Bool
isInvertingClause ct = case ct of
  YesNoQuestion -> True
  WhObject _    -> True
  WhAdjunct _   -> True
  _             -> False

isObjectGapClause :: ClauseType -> Bool
isObjectGapClause ct = case ct of
  WhObject _ -> True
  _          -> False

activeComps :: Bool -> Verb -> Arguments -> [String]
activeComps skipObj v args = case vTrans v of
  Intransitive      -> []
  Transitive        -> if skipObj then [] else maybeToList (npText <$> argObj args)
  Ditransitive      -> maybeToList (npText <$> argIndObj args)
                    ++ (if skipObj then [] else maybeToList (npText <$> argObj args))
  Predicative       -> maybeToList (argPred args)
  SubcatClause _    -> maybeToList (argClause args)

passiveComps :: Bool -> Verb -> Arguments -> [String]
passiveComps skipObj v args =
  let retainedObj =
        if vTrans v == Ditransitive && not skipObj
        then maybeToList (npText <$> argObj args)
        else []
      byPhrase = maybe [] (\b -> ["by", npText b]) (argBy args)
  in retainedObj ++ byPhrase

-- =========================================================
-- 3. The Builder Engine
-- =========================================================

data ClauseParts = ClauseParts
  { cpSubj  :: String
  , cpAux1  :: Maybe String
  , cpNeg   :: [String]
  , cpAuxR  :: [String]
  , cpMain  :: Maybe String
  , cpComp  :: [String]
  }

buildClauseParts :: Style -> SubjectNP -> Verb -> Arguments -> FiniteSpec -> Either String ClauseParts
buildClauseParts style subj v args s = do

  let objGap = isObjectGapClause (fsClauseType s)

  if fsProgressive s && not (vAllowsProg v)
    then Left "Verb blocks progressive aspect."
    else pure ()

  if fsVoice s == Passive && (vTrans v == Intransitive || vTrans v == Predicative)
    then Left "Intransitive/predicative verbs cannot be passivized."
    else pure ()

  if fsVoice s == Active then case vTrans v of
      Transitive ->
        if not objGap && isNothing (argObj args)
          then Left "Transitive active clause requires an object."
          else pure ()
      Predicative ->
        if isNothing (argPred args)
          then Left "Missing complement."
          else pure ()
      Ditransitive ->
        if isNothing (argIndObj args) || (not objGap && isNothing (argObj args))
          then Left "Missing ditransitive objects."
          else pure ()
      SubcatClause _ ->
        if isNothing (argClause args)
          then Left "Missing clausal complement."
          else pure ()
      Intransitive ->
        if isJust (argObj args)
          then Left "Intransitive verb cannot take a direct object."
          else pure ()
  else pure ()

  let npSubj = agreeNP subj args
      baseAux = allAuxFinite s
      requiresInversion = isInvertingClause (fsClauseType s)

  let (surfaceAux, isMainEmpty) =
        if null baseAux
        then if vClass v == Copular
             then ([ABe], True)
             else if fsPolarity s == Negative || requiresInversion
                  then ([ADo], False)
                  else ([], False)
        else (baseAux, False)

  let processRest _ [] = []
      processRest p (x:xs) = linkForm p x : processRest x xs

  let (aux1T, negT, auxR) = case surfaceAux of
        [] -> (Nothing, [], [])
        (a:as) ->
          let fw = realizeFirstAux (fsTense s) npSubj a
              rw = processRest a as
              (finalFw, finalNeg) =
                if fsPolarity s == Negative
                then case style of
                  Full -> (fw, ["not"])
                  Contracted ->
                    if fw == "am"
                    then (fw, ["not"])
                    else case words (contract fw) of
                           []     -> (fw, ["not"])
                           (x:xs) -> (x, xs)
                else (fw, [])
          in (Just finalFw, finalNeg, rw)

  let mainOpt = if isMainEmpty
                then Nothing
                else Just $ case surfaceAux of
                  []      -> finiteLex (fsTense s) npSubj v
                  (ADo:_) -> vBase v
                  _       -> mainForm (fsPerfect s) (fsProgressive s) (fsVoice s) v

  let skipObj = isObjectGapClause (fsClauseType s)
      comps = case fsVoice s of
        Passive -> passiveComps skipObj v args
        Active  -> activeComps skipObj v args

  pure $ ClauseParts
    (case subj of { WhSubjNP w -> w; ExplIt -> "it"; ExplThere -> "there"; RefNP np -> npText np })
    aux1T negT auxR mainOpt comps

-- =========================================================
-- 4. Syntax Renderers
-- =========================================================

tagPronoun :: SubjectNP -> String
tagPronoun (RefNP np)
  | npNumber np == Plural = "they"
  | npPerson np == First  = "I"
  | npPerson np == Second = "you"
  | otherwise             = "it"
tagPronoun ExplIt = "it"
tagPronoun ExplThere = "there"
tagPronoun (WhSubjNP _) = "it"

renderFinite :: Style -> SubjectNP -> Verb -> Arguments -> FiniteSpec -> Either String String
renderFinite style subj v args s = do
  cp <- buildClauseParts style subj v args s
  let punct = case fsClauseType s of { Declarative -> "."; _ -> "?" }
      strWords = case fsClauseType s of
        Declarative ->
           [cpSubj cp] ++ catMaybes [cpAux1 cp] ++ cpNeg cp ++ cpAuxR cp ++ catMaybes [cpMain cp] ++ cpComp cp
        YesNoQuestion ->
           catMaybes [cpAux1 cp] ++ [cpSubj cp] ++ cpNeg cp ++ cpAuxR cp ++ catMaybes [cpMain cp] ++ cpComp cp
        WhObject wh ->
           [wh] ++ catMaybes [cpAux1 cp] ++ [cpSubj cp] ++ cpNeg cp ++ cpAuxR cp ++ catMaybes [cpMain cp] ++ cpComp cp
        WhAdjunct wh ->
           [wh] ++ catMaybes [cpAux1 cp] ++ [cpSubj cp] ++ cpNeg cp ++ cpAuxR cp ++ catMaybes [cpMain cp] ++ cpComp cp
        WhSubject ->
           [cpSubj cp] ++ catMaybes [cpAux1 cp] ++ cpNeg cp ++ cpAuxR cp ++ catMaybes [cpMain cp] ++ cpComp cp
        TagQuestion ->
           let body = [cpSubj cp] ++ catMaybes [cpAux1 cp] ++ cpNeg cp ++ cpAuxR cp ++ catMaybes [cpMain cp] ++ cpComp cp
               oppPol = if fsPolarity s == Affirmative then Negative else Affirmative
               tagAuxBase = case allAuxFinite s of
                 (a:_) -> a
                 [] -> if vClass v == Copular then ABe else ADo
               npSubj = agreeNP subj args
               tagWBase = realizeFirstAux (fsTense s) npSubj tagAuxBase
               tagAuxFinal =
                 if oppPol == Negative
                 then if fsTense s == Present && tagWBase == "am" then "aren't" else contract tagWBase
                 else tagWBase
           in body ++ [",", tagAuxFinal, tagPronoun subj]

  let sentence = case fsClauseType s of
        TagQuestion -> let (b, t) = splitAt (length strWords - 3) strWords in unwords b ++ unwords t
        _ -> unwords strWords
  pure (sentence ++ punct)

compWord :: Complementizer -> String
compWord c = case c of { ThatC -> "that"; IfC -> "if"; WhetherC -> "whether"; BecauseC -> "because" }

relWord :: RelPron -> String
relWord r = case r of { ThatRel -> "that"; WhichRel -> "which"; WhoRel -> "who" }

renderEmbedded :: Style -> Complementizer -> SubjectNP -> Verb -> Arguments -> FiniteSpec -> Either String String
renderEmbedded style c subj v args s = do
  cp <- buildClauseParts style subj v args (s { fsClauseType = Declarative })
  let body = [cpSubj cp] ++ catMaybes [cpAux1 cp] ++ cpNeg cp ++ cpAuxR cp ++ catMaybes [cpMain cp] ++ cpComp cp
  pure $ unwords (compWord c : body)

renderRelative :: Style -> RelPron -> RelGap -> SubjectNP -> Verb -> Arguments -> FiniteSpec -> Either String String
renderRelative style rp gap subj v args s = do
   let s' = s { fsClauseType = case gap of { RelObj -> WhObject ""; RelSubj -> Declarative } }
   cp <- buildClauseParts style subj v args s'
   let body = case gap of
         RelObj  -> [cpSubj cp] ++ catMaybes [cpAux1 cp] ++ cpNeg cp ++ cpAuxR cp ++ catMaybes [cpMain cp] ++ cpComp cp
         RelSubj -> catMaybes [cpAux1 cp] ++ cpNeg cp ++ cpAuxR cp ++ catMaybes [cpMain cp] ++ cpComp cp
   pure $ unwords $ filter (not . null) (relWord rp : body)

renderImperative :: Style -> Verb -> Arguments -> ImperativeSpec -> Either String String
renderImperative style v args s = do
  if isProgressive s && not (vAllowsProg v) then Left "Verb blocks progressive aspect." else pure ()
  if isVoice s == Passive && (vTrans v == Intransitive || vTrans v == Predicative) then Left "Intransitive/predicative verbs cannot be passivized." else pure ()

  let baseAux = (if isPerfect s then [AHave] else [])
             ++ (if isProgressive s then [ABe] else [])
             ++ (if isVoice s == Passive then [ABe] else [])

  let processRest _ [] = []
      processRest p (x:xs) = linkForm p x : processRest x xs

  let realizeBareAux [] = []
      realizeBareAux (a:as) = first a : processRest a as
        where
          first AHave = "have"
          first ABe   = "be"
          first _     = ""

  let auxStrs = realizeBareAux baseAux
  let mainW = mainForm (isPerfect s) (isProgressive s) (isVoice s) v
  let comps = case isVoice s of
        Passive -> passiveComps False v args
        Active  -> activeComps False v args

  let affirmative =
        if null auxStrs
        then unwords (vBase v : comps) ++ "!"
        else unwords (auxStrs ++ [mainW] ++ comps) ++ "!"

  let negative =
        if null auxStrs
        then unwords (negateFirst style "do" ++ [vBase v] ++ comps) ++ "!"
        else unwords (negateFirst style "do" ++ auxStrs ++ [mainW] ++ comps) ++ "!"

  pure $ case isPolarity s of { Affirmative -> affirmative; Negative -> negative }

renderNonFinite :: Verb -> Arguments -> NonFinite -> Either String String
renderNonFinite v args nf = case nf of
  Infinitive perfect progressive voice pol -> do
    if progressive && not (vAllowsProg v) then Left "Verb blocks progressive aspect." else pure ()
    if voice == Passive && (vTrans v == Intransitive || vTrans v == Predicative) then Left "Intransitive/predicative verbs cannot be passivized." else pure ()

    let baseAux = (if perfect then [AHave] else [])
               ++ (if progressive then [ABe] else [])
               ++ (if voice == Passive then [ABe] else [])

    let processRest _ [] = []
        processRest p (x:xs) = linkForm p x : processRest x xs

    let auxStrs = case baseAux of
          []     -> []
          (a:as) -> (case a of { AHave -> "have"; ABe -> "be"; _ -> "" }) : processRest a as

    let mainW = mainForm perfect progressive voice v
    let comps = case voice of
          Passive -> passiveComps False v args
          Active  -> activeComps False v args

    let core = filter (not . null) $ ["to"] ++ auxStrs ++ [mainW] ++ comps
    pure $ case pol of { Affirmative -> unwords core; Negative -> unwords ("not" : core) }

  Gerund perfect voice pol -> do
    if voice == Passive && (vTrans v == Intransitive || vTrans v == Predicative) then Left "Intransitive/predicative verbs cannot be passivized." else pure ()
    let core = case (perfect, voice) of
          (False, Active)  -> [vPresPart v]
          (False, Passive) -> ["being", vPastPart v]
          (True,  Active)  -> ["having", vPastPart v]
          (True,  Passive) -> ["having", "been", vPastPart v]
    let comps = case voice of
          Active  -> activeComps False v args
          Passive -> passiveComps False v args
    pure $ unwords $ (if pol == Negative then ["not"] else []) ++ core ++ comps

  Participle pf pol -> do
    let voiceOf pf' = case pf' of
          PresParticipleActive -> Active
          PresParticiplePassive -> Passive
          PastParticiplePassive -> Passive
          PerfParticipleActive -> Active
          PerfParticiplePassive -> Passive

    let voice = voiceOf pf
    if voice == Passive && (vTrans v == Intransitive || vTrans v == Predicative) then Left "Intransitive/predicative verbs cannot be passivized." else pure ()

    let core = case pf of
          PresParticipleActive  -> [vPresPart v]
          PresParticiplePassive -> ["being", vPastPart v]
          PastParticiplePassive -> [vPastPart v]
          PerfParticipleActive  -> ["having", vPastPart v]
          PerfParticiplePassive -> ["having", "been", vPastPart v]

    let comps = case voice of
          Active  -> activeComps False v args
          Passive -> passiveComps False v args

    pure $ unwords $ (if pol == Negative then ["not"] else []) ++ core ++ comps

-- =========================================================
-- 5. Main Execution Block (Examples)
-- =========================================================

main :: IO ()
main = do
  let nullArgs = Arguments Nothing Nothing Nothing Nothing Nothing
  let ship  = RefNP (NP "the ship" Third Singular)
  let ships = RefNP (NP "the ships" Third Plural)
  let iSubj = RefNP (NP "I" First Singular)

  let beV     = Verb "be"     "is"      "was"     "been"    "being"    Copular Predicative True
  let scanV   = Verb "scan"   "scans"   "scanned" "scanned" "scanning" Lexical Transitive True
  let giveV   = Verb "give"   "gives"   "gave"    "given"   "giving"   Lexical Ditransitive True
  let arriveV = Verb "arrive" "arrives" "arrived" "arrived" "arriving" Lexical Intransitive True
  let knowV   = Verb "know"   "knows"   "knew"    "known"   "knowing"  Lexical (SubcatClause TakesThat) False
  let knowV2  = Verb "know"   "knows"   "knew"    "known"   "knowing"  Lexical Transitive False

  let activeSector = nullArgs { argObj = Just (NP "the sector" Third Singular) }
  let passiveShip  = nullArgs { argBy  = Just (NP "the ship" Third Singular) }

  putStrLn "--- Fix #1: WH inversion now works with real wh-words ---"
  print $ renderFinite Full ship scanV activeSector
    (FiniteSpec Present Nothing False False Active Affirmative (WhObject "what"))
  print $ renderFinite Full ship scanV activeSector
    (FiniteSpec Past Nothing False False Active Affirmative (WhAdjunct "why"))

  putStrLn "\n--- Fix #2: 'am' contraction in declaratives (no 'I aren't ...') ---"
  print $ renderFinite Contracted iSubj beV nullArgs{argPred = Just "ready"}
    (FiniteSpec Present Nothing False False Active Negative Declarative)
  print $ renderFinite Contracted iSubj beV nullArgs{argPred = Just "ready"}
    (FiniteSpec Present Nothing False False Active Affirmative TagQuestion)

  putStrLn "\n--- Fix #3: Object-gap clauses no longer require dummy object argument ---"
  print $ renderFinite Full ship scanV nullArgs
    (FiniteSpec Present Nothing False False Active Affirmative (WhObject "what"))
  print $ renderRelative Full ThatRel RelObj ship scanV nullArgs
    (FiniteSpec Past Nothing False False Active Affirmative Declarative)

  putStrLn "\n--- Fix #4: Full non-finite support (Infinitive / Gerund / Participle) ---"
  print $ renderNonFinite scanV activeSector (Infinitive True True Active Negative)
  print $ renderNonFinite scanV activeSector (Gerund False Active Affirmative)
  print $ renderNonFinite scanV passiveShip  (Gerund True Passive Negative)
  print $ renderNonFinite scanV activeSector (Participle PresParticipleActive Affirmative)
  print $ renderNonFinite scanV passiveShip  (Participle PerfParticiplePassive Negative)

  putStrLn "\n--- Original-style baseline checks ---"
  print $ renderFinite Full ship beV nullArgs{argPred = Just "safe"}
    (FiniteSpec Present Nothing False False Active Affirmative YesNoQuestion)

  print $ renderFinite Full ExplThere beV nullArgs{argObj = Just (NP "ships" Third Plural)}
    (FiniteSpec Present Nothing False False Active Affirmative Declarative)

  print $ renderFinite Contracted ExplThere beV nullArgs{argObj = Just (NP "a base" Third Singular)}
    (FiniteSpec Past Nothing False False Active Negative Declarative)

  print $ renderFinite Full ship knowV nullArgs{argClause = Just "that the sector is safe"}
    (FiniteSpec Present Nothing False False Active Affirmative Declarative)

  print $ renderFinite Full (WhSubjNP "what") scanV nullArgs{argObj = Just (NP "it" Third Singular)}
    (FiniteSpec Past Nothing False False Active Affirmative WhSubject)

  print $ renderFinite Full ships scanV nullArgs{argObj = Just (NP "the sector" Third Singular)}
    (FiniteSpec Past Nothing False False Active Affirmative TagQuestion)

  print $ renderFinite Full ship scanV activeSector
    (FiniteSpec Present Nothing True True Active Negative Declarative)

  print $ renderFinite Contracted ship scanV activeSector
    (FiniteSpec Present Nothing False False Active Negative Declarative)

  print $ renderEmbedded Full ThatC ship scanV activeSector
    (FiniteSpec Past Nothing False False Active Affirmative Declarative)

  print $ renderRelative Full ThatRel RelSubj ship scanV activeSector
    (FiniteSpec Present Nothing False False Active Affirmative Declarative)

  print $ renderImperative Full scanV passiveShip
    (ImperativeSpec False False Passive Affirmative)

  print $ renderFinite Full ship arriveV nullArgs
    (FiniteSpec Past Nothing False False Passive Affirmative Declarative)

  print $ renderFinite Full ship knowV2 activeSector
    (FiniteSpec Present Nothing False True Active Affirmative Declarative)

  print $ renderFinite Full ship giveV nullArgs { argIndObj = Just (NP "the pilot" Third Singular), argObj = Just (NP "a map" Third Singular) }
    (FiniteSpec Past Nothing False False Active Affirmative Declarative)
