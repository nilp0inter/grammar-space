module Grammar.Types exposing (..)


type Tense
    = Past
    | Present
    | Future


type Person
    = First
    | Second
    | Third


type Number
    = Singular
    | Plural


type Voice
    = Active
    | Passive


type Polarity
    = Affirmative
    | Negative


type Style
    = Full
    | Contracted


type Modality
    = Can
    | Should
    | Must
    | Would
    | May
    | Might
    | Will


type VerbClass
    = Lexical
    | Copular


type ClauseForm
    = TakesThat
    | TakesInf
    | TakesGerund


type Transitivity
    = Intransitive
    | Transitive
    | Ditransitive
    | Predicative
    | SubcatClause ClauseForm


type alias NP =
    { npText : String
    , npPerson : Person
    , npNumber : Number
    }


type SubjectNP
    = RefNP NP
    | ExplIt
    | ExplThere
    | WhSubjNP String


type alias Verb =
    { vBase : String
    , v3sg : String
    , vPast : String
    , vPastPart : String
    , vPresPart : String
    , vClass : VerbClass
    , vTrans : Transitivity
    , vAllowsProg : Bool
    }


type alias Arguments =
    { argObj : Maybe NP
    , argIndObj : Maybe NP
    , argBy : Maybe NP
    , argPred : Maybe String
    , argClause : Maybe String
    }


type ClauseType
    = Declarative
    | YesNoQuestion
    | WhObject String
    | WhAdjunct String
    | WhSubject
    | TagQuestion


type alias FiniteSpec =
    { fsTense : Tense
    , fsModal : Maybe Modality
    , fsPerfect : Bool
    , fsProgressive : Bool
    , fsVoice : Voice
    , fsPolarity : Polarity
    , fsClauseType : ClauseType
    }


type Complementizer
    = ThatC
    | IfC
    | WhetherC
    | BecauseC


type RelPron
    = ThatRel
    | WhichRel
    | WhoRel


type RelGap
    = RelSubj
    | RelObj


type alias ImperativeSpec =
    { isPerfect : Bool
    , isProgressive : Bool
    , isVoice : Voice
    , isPolarity : Polarity
    }


type ParticipleForm
    = PresParticipleActive
    | PresParticiplePassive
    | PastParticiplePassive
    | PerfParticipleActive
    | PerfParticiplePassive


type NonFinite
    = Infinitive { perfect : Bool, progressive : Bool, voice : Voice, polarity : Polarity }
    | Gerund { perfect : Bool, voice : Voice, polarity : Polarity }
    | Participle { form : ParticipleForm, polarity : Polarity }


type Aux
    = AModal String
    | AHave
    | ABe
    | ADo


type alias ClauseParts =
    { cpSubj : String
    , cpAux1 : Maybe String
    , cpNeg : List String
    , cpAuxR : List String
    , cpMain : Maybe String
    , cpComp : List String
    }



-- UI Types


type ClauseMode
    = FiniteMode
    | ImperativeMode
    | InfinitiveMode
    | GerundMode
    | ParticipleMode


type WordRole
    = SubjectWord
    | Aux1Word
    | NegWord
    | AuxRWord
    | MainVerbWord
    | ComplementWord
    | PunctuationWord
    | WhWord
    | ComplementizerWord
    | TagWord
    | RelWord


type alias SentenceWord =
    { text : String
    , role : WordRole
    }



-- Verb Tense (12 combinations of Tense × Aspect)


type VerbTense
    = SimplePast
    | PastContinuous
    | PastPerfect
    | PastPerfectContinuous
    | SimplePresent
    | PresentContinuous
    | PresentPerfect
    | PresentPerfectContinuous
    | SimpleFuture
    | FutureContinuous
    | FuturePerfect
    | FuturePerfectContinuous


allVerbTenses : List VerbTense
allVerbTenses =
    [ SimplePast
    , PastContinuous
    , PastPerfect
    , PastPerfectContinuous
    , SimplePresent
    , PresentContinuous
    , PresentPerfect
    , PresentPerfectContinuous
    , SimpleFuture
    , FutureContinuous
    , FuturePerfect
    , FuturePerfectContinuous
    ]


verbTenseToSpec : VerbTense -> { tense : Tense, perfect : Bool, progressive : Bool }
verbTenseToSpec vt =
    case vt of
        SimplePast ->
            { tense = Past, perfect = False, progressive = False }

        PastContinuous ->
            { tense = Past, perfect = False, progressive = True }

        PastPerfect ->
            { tense = Past, perfect = True, progressive = False }

        PastPerfectContinuous ->
            { tense = Past, perfect = True, progressive = True }

        SimplePresent ->
            { tense = Present, perfect = False, progressive = False }

        PresentContinuous ->
            { tense = Present, perfect = False, progressive = True }

        PresentPerfect ->
            { tense = Present, perfect = True, progressive = False }

        PresentPerfectContinuous ->
            { tense = Present, perfect = True, progressive = True }

        SimpleFuture ->
            { tense = Future, perfect = False, progressive = False }

        FutureContinuous ->
            { tense = Future, perfect = False, progressive = True }

        FuturePerfect ->
            { tense = Future, perfect = True, progressive = False }

        FuturePerfectContinuous ->
            { tense = Future, perfect = True, progressive = True }


specToVerbTense : Tense -> Bool -> Bool -> VerbTense
specToVerbTense tense perfect progressive =
    case ( tense, perfect, progressive ) of
        ( Past, False, False ) ->
            SimplePast

        ( Past, False, True ) ->
            PastContinuous

        ( Past, True, False ) ->
            PastPerfect

        ( Past, True, True ) ->
            PastPerfectContinuous

        ( Present, False, False ) ->
            SimplePresent

        ( Present, False, True ) ->
            PresentContinuous

        ( Present, True, False ) ->
            PresentPerfect

        ( Present, True, True ) ->
            PresentPerfectContinuous

        ( Future, False, False ) ->
            SimpleFuture

        ( Future, False, True ) ->
            FutureContinuous

        ( Future, True, False ) ->
            FuturePerfect

        ( Future, True, True ) ->
            FuturePerfectContinuous


verbTenseFromString : String -> Maybe VerbTense
verbTenseFromString s =
    case s of
        "SimplePast" ->
            Just SimplePast

        "PastContinuous" ->
            Just PastContinuous

        "PastPerfect" ->
            Just PastPerfect

        "PastPerfectContinuous" ->
            Just PastPerfectContinuous

        "SimplePresent" ->
            Just SimplePresent

        "PresentContinuous" ->
            Just PresentContinuous

        "PresentPerfect" ->
            Just PresentPerfect

        "PresentPerfectContinuous" ->
            Just PresentPerfectContinuous

        "SimpleFuture" ->
            Just SimpleFuture

        "FutureContinuous" ->
            Just FutureContinuous

        "FuturePerfect" ->
            Just FuturePerfect

        "FuturePerfectContinuous" ->
            Just FuturePerfectContinuous

        _ ->
            Nothing


verbTenseToString : VerbTense -> String
verbTenseToString vt =
    case vt of
        SimplePast ->
            "SimplePast"

        PastContinuous ->
            "PastContinuous"

        PastPerfect ->
            "PastPerfect"

        PastPerfectContinuous ->
            "PastPerfectContinuous"

        SimplePresent ->
            "SimplePresent"

        PresentContinuous ->
            "PresentContinuous"

        PresentPerfect ->
            "PresentPerfect"

        PresentPerfectContinuous ->
            "PresentPerfectContinuous"

        SimpleFuture ->
            "SimpleFuture"

        FutureContinuous ->
            "FutureContinuous"

        FuturePerfect ->
            "FuturePerfect"

        FuturePerfectContinuous ->
            "FuturePerfectContinuous"


verbTenseLabel : VerbTense -> String
verbTenseLabel vt =
    case vt of
        SimplePast ->
            "Simple Past"

        PastContinuous ->
            "Past Continuous"

        PastPerfect ->
            "Past Perfect"

        PastPerfectContinuous ->
            "Past Perfect Continuous"

        SimplePresent ->
            "Simple Present"

        PresentContinuous ->
            "Present Continuous"

        PresentPerfect ->
            "Present Perfect"

        PresentPerfectContinuous ->
            "Present Perfect Continuous"

        SimpleFuture ->
            "Simple Future"

        FutureContinuous ->
            "Future Continuous"

        FuturePerfect ->
            "Future Perfect"

        FuturePerfectContinuous ->
            "Future Perfect Continuous"


type Aspect
    = Simple
    | Continuous
    | PerfectAspect
    | PerfectContinuousAspect


allAspects : List Aspect
allAspects =
    [ Simple, Continuous, PerfectAspect, PerfectContinuousAspect ]


allTenses : List Tense
allTenses =
    [ Past, Present, Future ]


aspectLabel : Aspect -> String
aspectLabel a =
    case a of
        Simple ->
            "Simple"

        Continuous ->
            "Continuous"

        PerfectAspect ->
            "Perfect"

        PerfectContinuousAspect ->
            "Perfect Continuous"


tenseLabel : Tense -> String
tenseLabel t =
    case t of
        Past ->
            "Past"

        Present ->
            "Present"

        Future ->
            "Future"


verbTenseForCell : Tense -> Aspect -> VerbTense
verbTenseForCell t a =
    case ( t, a ) of
        ( Past, Simple ) ->
            SimplePast

        ( Past, Continuous ) ->
            PastContinuous

        ( Past, PerfectAspect ) ->
            PastPerfect

        ( Past, PerfectContinuousAspect ) ->
            PastPerfectContinuous

        ( Present, Simple ) ->
            SimplePresent

        ( Present, Continuous ) ->
            PresentContinuous

        ( Present, PerfectAspect ) ->
            PresentPerfect

        ( Present, PerfectContinuousAspect ) ->
            PresentPerfectContinuous

        ( Future, Simple ) ->
            SimpleFuture

        ( Future, Continuous ) ->
            FutureContinuous

        ( Future, PerfectAspect ) ->
            FuturePerfect

        ( Future, PerfectContinuousAspect ) ->
            FuturePerfectContinuous


verbTensesForTense : Tense -> List VerbTense
verbTensesForTense t =
    List.map (verbTenseForCell t) allAspects


verbTensesForAspect : Aspect -> List VerbTense
verbTensesForAspect a =
    List.map (\t -> verbTenseForCell t a) allTenses
