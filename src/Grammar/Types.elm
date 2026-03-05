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
