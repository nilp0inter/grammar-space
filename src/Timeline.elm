module Timeline exposing (view)

import Animator
import Grammar.Types exposing (Modality(..), Polarity(..), Tense(..), VerbTense(..), Voice(..), specToVerbTense, verbTenseLabel, verbTenseToSpec)
import Html exposing (Html)
import Html.Attributes
import Svg exposing (svg)
import Svg.Attributes as SA
import Svg.Events as SE


type alias Config msg =
    { tenseTimeline : Animator.Timeline Tense
    , tense : Tense
    , perfect : Bool
    , progressive : Bool
    , voice : Voice
    , polarity : Polarity
    , modal : Maybe Modality
    , onSelectVerbTense : VerbTense -> msg
    }



-- Coordinates


pastX : Float
pastX =
    150


presentX : Float
presentX =
    350


futureX : Float
futureX =
    550


axisY : Float
axisY =
    200


tenseToX : Tense -> Float
tenseToX tense =
    case tense of
        Past ->
            pastX

        Present ->
            presentX

        Future ->
            futureX



-- Row Y positions (4 aspect rows above the axis)


simpleY : Float
simpleY =
    170


continuousY : Float
continuousY =
    130


perfectY : Float
perfectY =
    90


perfectContinuousY : Float
perfectContinuousY =
    50



-- Colors


slateAxis : String
slateAxis =
    "#475569"


slateLabel : String
slateLabel =
    "#94a3b8"


indigoAccent : String
indigoAccent =
    "#818cf8"


emeraldFill : String
emeraldFill =
    "#34d399"


emeraldFillAlpha : String
emeraldFillAlpha =
    "rgba(52, 211, 153, 0.4)"


dimColor : String
dimColor =
    "#4a5568"


dimOpacity : String
dimOpacity =
    "0.5"


selectedHighlightFill : String
selectedHighlightFill =
    "rgba(99, 102, 241, 0.12)"



-- View


view : Config msg -> Html msg
view config =
    let
        arrowX =
            Animator.move config.tenseTimeline
                (\tense ->
                    Animator.at (tenseToX tense)
                )

        selectedVT =
            specToVerbTense config.tense config.perfect config.progressive
    in
    Html.div [ Html.Attributes.class "w-full flex justify-center" ]
        [ svg
            [ SA.viewBox "0 0 700 320"
            , SA.width "700"
            , SA.class "max-w-full h-auto"
            , SA.style "user-select: none"
            ]
            [ viewRowLabels
            , viewAllIndicators selectedVT config
            , viewAxis
            , viewTickMarks
            , viewLabels
            , viewNowLine
            , viewArrow arrowX
            , viewTenseNameLabel arrowX selectedVT
            , viewBadges arrowX config.voice config.polarity config.modal
            ]
        ]



-- Row labels on left side


viewRowLabels : Svg.Svg msg
viewRowLabels =
    Svg.g
        [ SA.fontFamily "system-ui, sans-serif"
        , SA.fontSize "10"
        , SA.fontWeight "600"
        , SA.textAnchor "end"
        , SA.fill slateLabel
        ]
        [ Svg.text_
            [ SA.x "65"
            , SA.y (ff (simpleY + 4))
            ]
            [ Svg.text "SIMPLE" ]
        , Svg.text_
            [ SA.x "65"
            , SA.y (ff (continuousY + 4))
            ]
            [ Svg.text "CONTINUOUS" ]
        , Svg.text_
            [ SA.x "65"
            , SA.y (ff (perfectY + 4))
            ]
            [ Svg.text "PERFECT" ]
        , Svg.text_
            [ SA.x "65"
            , SA.y (ff (perfectContinuousY + 4))
            ]
            [ Svg.text "PERF. CONT." ]
        ]



-- All 12 indicators


viewAllIndicators : VerbTense -> Config msg -> Svg.Svg msg
viewAllIndicators selectedVT config =
    Svg.g []
        (List.map
            (\vt ->
                let
                    spec =
                        verbTenseToSpec vt

                    x =
                        tenseToX spec.tense

                    y =
                        aspectToY spec.perfect spec.progressive

                    isSelected =
                        vt == selectedVT
                in
                viewIndicator x y isSelected vt spec.perfect spec.progressive config
            )
            allVerbTensesList
        )


allVerbTensesList : List VerbTense
allVerbTensesList =
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


aspectToY : Bool -> Bool -> Float
aspectToY perfect progressive =
    case ( perfect, progressive ) of
        ( False, False ) ->
            simpleY

        ( False, True ) ->
            continuousY

        ( True, False ) ->
            perfectY

        ( True, True ) ->
            perfectContinuousY



-- Single indicator (shape + highlight + hitbox)


viewIndicator : Float -> Float -> Bool -> VerbTense -> Bool -> Bool -> Config msg -> Svg.Svg msg
viewIndicator x y isSelected vt perfect progressive config =
    let
        fillColor =
            if isSelected then
                emeraldFill

            else
                dimColor

        fillAlpha =
            if isSelected then
                emeraldFillAlpha

            else
                "rgba(74, 85, 104, 0.25)"

        groupOpacity =
            if isSelected then
                "1"

            else
                dimOpacity
    in
    Svg.g []
        [ if isSelected then
            Svg.rect
                [ SA.x (ff (x - 40))
                , SA.y (ff (y - 15))
                , SA.width "80"
                , SA.height "30"
                , SA.rx "6"
                , SA.fill selectedHighlightFill
                ]
                []

          else
            Svg.g [] []
        , Svg.g [ SA.opacity groupOpacity ]
            [ viewIndicatorShape x y fillColor fillAlpha perfect progressive ]
        , Svg.rect
            [ SA.x (ff (x - 40))
            , SA.y (ff (y - 15))
            , SA.width "80"
            , SA.height "30"
            , SA.fill "transparent"
            , SA.cursor "pointer"
            , SE.onClick (config.onSelectVerbTense vt)
            ]
            []
        ]



-- Indicator shape based on aspect


viewIndicatorShape : Float -> Float -> String -> String -> Bool -> Bool -> Svg.Svg msg
viewIndicatorShape cx cy color colorAlpha perfect progressive =
    case ( perfect, progressive ) of
        ( False, False ) ->
            viewSimpleDot cx cy color

        ( False, True ) ->
            viewProgressiveBar cx cy color colorAlpha

        ( True, False ) ->
            viewPerfectShape cx cy color

        ( True, True ) ->
            viewPerfectProgressiveShape cx cy color colorAlpha



-- Simple: filled circle (point event)


viewSimpleDot : Float -> Float -> String -> Svg.Svg msg
viewSimpleDot cx cy color =
    Svg.circle
        [ SA.cx (ff cx)
        , SA.cy (ff cy)
        , SA.r "8"
        , SA.fill color
        ]
        []



-- Progressive: elongated rounded bar with wavy interior


viewProgressiveBar : Float -> Float -> String -> String -> Svg.Svg msg
viewProgressiveBar cx cy color colorAlpha =
    let
        barW =
            60

        barH =
            16

        x =
            cx - barW / 2

        y =
            cy - barH / 2

        waveYpos =
            cy
    in
    Svg.g []
        [ Svg.rect
            [ SA.x (ff x)
            , SA.y (ff y)
            , SA.width (ff barW)
            , SA.height (ff barH)
            , SA.rx "8"
            , SA.fill colorAlpha
            , SA.stroke color
            , SA.strokeWidth "1.5"
            ]
            []
        , Svg.path
            [ SA.d (wavePath (x + 8) waveYpos (barW - 16) 3 4)
            , SA.stroke color
            , SA.strokeWidth "1.5"
            , SA.fill "none"
            , SA.strokeLinecap "round"
            ]
            []
        ]



-- Perfect: line ending at a filled circle


viewPerfectShape : Float -> Float -> String -> Svg.Svg msg
viewPerfectShape cx cy color =
    let
        lineLen =
            30
    in
    Svg.g []
        [ Svg.line
            [ SA.x1 (ff (cx - lineLen))
            , SA.y1 (ff cy)
            , SA.x2 (ff cx)
            , SA.y2 (ff cy)
            , SA.stroke color
            , SA.strokeWidth "2"
            ]
            []
        , Svg.circle
            [ SA.cx (ff cx)
            , SA.cy (ff cy)
            , SA.r "6"
            , SA.fill color
            ]
            []
        ]



-- Perfect Progressive: rounded bar ending at a circle


viewPerfectProgressiveShape : Float -> Float -> String -> String -> Svg.Svg msg
viewPerfectProgressiveShape cx cy color colorAlpha =
    let
        barW =
            50

        barH =
            16

        barX =
            cx - barW - 4

        y =
            cy - barH / 2

        waveYpos =
            cy
    in
    Svg.g []
        [ Svg.rect
            [ SA.x (ff barX)
            , SA.y (ff y)
            , SA.width (ff barW)
            , SA.height (ff barH)
            , SA.rx "8"
            , SA.fill colorAlpha
            , SA.stroke color
            , SA.strokeWidth "1.5"
            ]
            []
        , Svg.path
            [ SA.d (wavePath (barX + 8) waveYpos (barW - 16) 3 4)
            , SA.stroke color
            , SA.strokeWidth "1.5"
            , SA.fill "none"
            , SA.strokeLinecap "round"
            ]
            []
        , Svg.circle
            [ SA.cx (ff (cx + 4))
            , SA.cy (ff cy)
            , SA.r "6"
            , SA.fill color
            ]
            []
        , Svg.line
            [ SA.x1 (ff (barX + barW))
            , SA.y1 (ff cy)
            , SA.x2 (ff (cx + 4 - 6))
            , SA.y2 (ff cy)
            , SA.stroke color
            , SA.strokeWidth "2"
            ]
            []
        ]



-- Wave path helper


wavePath : Float -> Float -> Float -> Float -> Int -> String
wavePath startX y totalW amplitude segments =
    let
        segW =
            totalW / toFloat segments

        buildSeg i =
            let
                x1 =
                    startX + toFloat i * segW + segW * 0.25

                y1 =
                    if modBy 2 i == 0 then
                        y - amplitude

                    else
                        y + amplitude

                x2 =
                    startX + toFloat i * segW + segW * 0.75

                y2 =
                    if modBy 2 i == 0 then
                        y + amplitude

                    else
                        y - amplitude

                endX =
                    startX + toFloat (i + 1) * segW

                endY =
                    y
            in
            "C " ++ ff x1 ++ " " ++ ff y1 ++ ", " ++ ff x2 ++ " " ++ ff y2 ++ ", " ++ ff endX ++ " " ++ ff endY
    in
    "M " ++ ff startX ++ " " ++ ff y ++ " " ++ String.join " " (List.map buildSeg (List.range 0 (segments - 1)))


ff : Float -> String
ff =
    String.fromFloat



-- Axis line


viewAxis : Svg.Svg msg
viewAxis =
    Svg.line
        [ SA.x1 "50"
        , SA.y1 (ff axisY)
        , SA.x2 "650"
        , SA.y2 (ff axisY)
        , SA.stroke slateAxis
        , SA.strokeWidth "2"
        ]
        []



-- Tick marks


viewTickMarks : Svg.Svg msg
viewTickMarks =
    Svg.g []
        [ tickMark pastX
        , tickMark presentX
        , tickMark futureX
        ]


tickMark : Float -> Svg.Svg msg
tickMark x =
    Svg.line
        [ SA.x1 (ff x)
        , SA.y1 (ff (axisY - 8))
        , SA.x2 (ff x)
        , SA.y2 (ff (axisY + 8))
        , SA.stroke slateAxis
        , SA.strokeWidth "2"
        ]
        []



-- Labels


viewLabels : Svg.Svg msg
viewLabels =
    Svg.g
        [ SA.fontFamily "system-ui, sans-serif"
        , SA.fontSize "13"
        , SA.fontWeight "600"
        , SA.textAnchor "middle"
        ]
        [ Svg.text_
            [ SA.x (ff pastX)
            , SA.y (ff (axisY + 25))
            , SA.fill slateLabel
            ]
            [ Svg.text "PAST" ]
        , Svg.text_
            [ SA.x (ff presentX)
            , SA.y (ff (axisY + 25))
            , SA.fill indigoAccent
            ]
            [ Svg.text "NOW" ]
        , Svg.text_
            [ SA.x (ff futureX)
            , SA.y (ff (axisY + 25))
            , SA.fill slateLabel
            ]
            [ Svg.text "FUTURE" ]
        ]



-- NOW dashed line


viewNowLine : Svg.Svg msg
viewNowLine =
    Svg.line
        [ SA.x1 (ff presentX)
        , SA.y1 "30"
        , SA.x2 (ff presentX)
        , SA.y2 (ff (axisY - 10))
        , SA.stroke indigoAccent
        , SA.strokeWidth "1"
        , SA.strokeDasharray "4 3"
        , SA.opacity "0.5"
        ]
        []



-- Arrow pointer


viewArrow : Float -> Svg.Svg msg
viewArrow arrowX =
    let
        tipY =
            axisY + 14

        baseY =
            axisY + 28

        halfW =
            8

        points =
            ff arrowX
                ++ ","
                ++ ff tipY
                ++ " "
                ++ ff (arrowX - halfW)
                ++ ","
                ++ ff baseY
                ++ " "
                ++ ff (arrowX + halfW)
                ++ ","
                ++ ff baseY
    in
    Svg.polygon
        [ SA.points points
        , SA.fill indigoAccent
        ]
        []



-- Tense name label


viewTenseNameLabel : Float -> VerbTense -> Svg.Svg msg
viewTenseNameLabel arrowX selectedVT =
    Svg.text_
        [ SA.x (ff arrowX)
        , SA.y "255"
        , SA.textAnchor "middle"
        , SA.fill indigoAccent
        , SA.fontSize "16"
        , SA.fontWeight "700"
        , SA.fontFamily "system-ui, sans-serif"
        ]
        [ Svg.text (verbTenseLabel selectedVT) ]



-- Feature badges


viewBadges : Float -> Voice -> Polarity -> Maybe Modality -> Svg.Svg msg
viewBadges arrowX voice polarity modal =
    let
        badgeYpos =
            275

        badges =
            List.filterMap identity
                [ if voice == Passive then
                    Just ( "PASSIVE", "#fda4af" )

                  else
                    Nothing
                , if polarity == Negative then
                    Just ( "NEG", "#fb7185" )

                  else
                    Nothing
                , case modal of
                    Just m ->
                        Just ( modalLabel m, "#fcd34d" )

                    Nothing ->
                        Nothing
                ]

        totalBadges =
            List.length badges

        spacing =
            70

        startX =
            arrowX - toFloat (totalBadges - 1) * spacing / 2
    in
    Svg.g []
        (List.indexedMap
            (\i ( label, color ) ->
                viewBadge (startX + toFloat i * spacing) (toFloat badgeYpos) label color
            )
            badges
        )


viewBadge : Float -> Float -> String -> String -> Svg.Svg msg
viewBadge cx cy label color =
    let
        textW =
            toFloat (String.length label) * 7.5 + 16

        rectX =
            cx - textW / 2

        rectY =
            cy - 10
    in
    Svg.g []
        [ Svg.rect
            [ SA.x (ff rectX)
            , SA.y (ff rectY)
            , SA.width (ff textW)
            , SA.height "20"
            , SA.rx "10"
            , SA.fill "rgba(0, 0, 0, 0.3)"
            , SA.stroke color
            , SA.strokeWidth "1"
            ]
            []
        , Svg.text_
            [ SA.x (ff cx)
            , SA.y (ff (cy + 4))
            , SA.textAnchor "middle"
            , SA.fill color
            , SA.fontSize "11"
            , SA.fontWeight "600"
            , SA.fontFamily "system-ui, sans-serif"
            ]
            [ Svg.text label ]
        ]


modalLabel : Modality -> String
modalLabel m =
    case m of
        Can ->
            "can"

        Should ->
            "should"

        Must ->
            "must"

        Would ->
            "would"

        May ->
            "may"

        Might ->
            "might"

        Will ->
            "will"
