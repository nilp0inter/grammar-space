module Grammar.Lexicon exposing
    ( defaultArguments
    , subjectLabel
    , subjects
    , verbLabel
    , verbs
    )

import Grammar.Engine exposing (nullArgs)
import Grammar.Types exposing (..)



-- =========================================================
-- Preset Verbs
-- =========================================================


beV : Verb
beV =
    Verb "be" "is" "was" "been" "being" Copular Predicative True


scanV : Verb
scanV =
    Verb "scan" "scans" "scanned" "scanned" "scanning" Lexical Transitive True


giveV : Verb
giveV =
    Verb "give" "gives" "gave" "given" "giving" Lexical Ditransitive True


arriveV : Verb
arriveV =
    Verb "arrive" "arrives" "arrived" "arrived" "arriving" Lexical Intransitive True


knowV : Verb
knowV =
    Verb "know" "knows" "knew" "known" "knowing" Lexical (SubcatClause TakesThat) False


eatV : Verb
eatV =
    Verb "eat" "eats" "ate" "eaten" "eating" Lexical Transitive True


writeV : Verb
writeV =
    Verb "write" "writes" "wrote" "written" "writing" Lexical Transitive True


runV : Verb
runV =
    Verb "run" "runs" "ran" "run" "running" Lexical Intransitive True


seemV : Verb
seemV =
    Verb "seem" "seems" "seemed" "seemed" "seeming" Copular Predicative False


tellV : Verb
tellV =
    Verb "tell" "tells" "told" "told" "telling" Lexical Ditransitive True


wantV : Verb
wantV =
    Verb "want" "wants" "wanted" "wanted" "wanting" Lexical (SubcatClause TakesInf) True


verbs : List Verb
verbs =
    [ beV, scanV, giveV, arriveV, knowV, eatV, writeV, runV, seemV, tellV, wantV ]


verbLabel : Verb -> String
verbLabel v =
    v.vBase



-- =========================================================
-- Preset Subjects
-- =========================================================


subjects : List ( String, SubjectNP )
subjects =
    [ ( "I", RefNP (NP "I" First Singular) )
    , ( "you", RefNP (NP "you" Second Singular) )
    , ( "he", RefNP (NP "he" Third Singular) )
    , ( "she", RefNP (NP "she" Third Singular) )
    , ( "it", ExplIt )
    , ( "we", RefNP (NP "we" First Plural) )
    , ( "they", RefNP (NP "they" Third Plural) )
    , ( "the ship", RefNP (NP "the ship" Third Singular) )
    , ( "the ships", RefNP (NP "the ships" Third Plural) )
    , ( "there", ExplThere )
    ]


subjectLabel : ( String, SubjectNP ) -> String
subjectLabel ( label, _ ) =
    label



-- =========================================================
-- Default Arguments per Transitivity
-- =========================================================


defaultArguments : Verb -> Arguments
defaultArguments v =
    case v.vTrans of
        Intransitive ->
            nullArgs

        Transitive ->
            { nullArgs | argObj = Just (NP "a book" Third Singular) }

        Ditransitive ->
            { nullArgs
                | argObj = Just (NP "a map" Third Singular)
                , argIndObj = Just (NP "the pilot" Third Singular)
            }

        Predicative ->
            { nullArgs | argPred = Just "ready" }

        SubcatClause TakesThat ->
            { nullArgs | argClause = Just "that the sector is safe" }

        SubcatClause TakesInf ->
            { nullArgs | argClause = Just "to leave early" }

        SubcatClause TakesGerund ->
            { nullArgs | argClause = Just "leaving early" }
