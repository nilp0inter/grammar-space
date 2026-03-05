module Timeline exposing (view)

import Animator
import Grammar.Types exposing (Modality(..), Polarity(..), Tense(..), Voice(..))
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
    , onSetTense : Tense -> msg
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
    110


tenseToX : Tense -> Float
tenseToX tense =
    case tense of
        Past ->
            pastX

        Present ->
            presentX

        Future ->
            futureX



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


selectedZoneFill : String
selectedZoneFill =
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
    in
    Html.div [ Html.Attributes.class "w-full flex justify-center" ]
        [ svg
            [ SA.viewBox "0 0 700 180"
            , SA.width "700"
            , SA.class "max-w-full h-auto"
            , SA.style "user-select: none"
            ]
            [ viewSelectedZone config.tense
            , viewClickableZones config
            , viewAxis
            , viewTickMarks
            , viewLabels
            , viewNowLine
            , viewAspectShape arrowX config.perfect config.progressive
            , viewArrow arrowX
            , viewBadges arrowX config.voice config.polarity config.modal
            ]
        ]



-- Selected zone highlight


viewSelectedZone : Tense -> Svg.Svg msg
viewSelectedZone tense =
    let
        zoneX =
            case tense of
                Past ->
                    17

                Present ->
                    234

                Future ->
                    450

        zoneW =
            case tense of
                Past ->
                    216

                Present ->
                    216

                Future ->
                    233
    in
    Svg.rect
        [ SA.x (String.fromFloat zoneX)
        , SA.y "20"
        , SA.width (String.fromFloat zoneW)
        , SA.height "130"
        , SA.rx "8"
        , SA.fill selectedZoneFill
        ]
        []



-- Clickable zones


viewClickableZones : Config msg -> Svg.Svg msg
viewClickableZones config =
    Svg.g []
        [ clickableZone 17 216 (config.onSetTense Past)
        , clickableZone 234 216 (config.onSetTense Present)
        , clickableZone 450 233 (config.onSetTense Future)
        ]


clickableZone : Float -> Float -> msg -> Svg.Svg msg
clickableZone x w msg =
    Svg.rect
        [ SA.x (String.fromFloat x)
        , SA.y "20"
        , SA.width (String.fromFloat w)
        , SA.height "130"
        , SA.fill "transparent"
        , SA.cursor "pointer"
        , SE.onClick msg
        ]
        []



-- Axis line


viewAxis : Svg.Svg msg
viewAxis =
    Svg.line
        [ SA.x1 "50"
        , SA.y1 (String.fromFloat axisY)
        , SA.x2 "650"
        , SA.y2 (String.fromFloat axisY)
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
        [ SA.x1 (String.fromFloat x)
        , SA.y1 (String.fromFloat (axisY - 8))
        , SA.x2 (String.fromFloat x)
        , SA.y2 (String.fromFloat (axisY + 8))
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
            [ SA.x (String.fromFloat pastX)
            , SA.y "38"
            , SA.fill slateLabel
            ]
            [ Svg.text "PAST" ]
        , Svg.text_
            [ SA.x (String.fromFloat presentX)
            , SA.y "38"
            , SA.fill indigoAccent
            ]
            [ Svg.text "NOW" ]
        , Svg.text_
            [ SA.x (String.fromFloat futureX)
            , SA.y "38"
            , SA.fill slateLabel
            ]
            [ Svg.text "FUTURE" ]
        ]



-- NOW dashed line


viewNowLine : Svg.Svg msg
viewNowLine =
    Svg.line
        [ SA.x1 (String.fromFloat presentX)
        , SA.y1 "44"
        , SA.x2 (String.fromFloat presentX)
        , SA.y2 (String.fromFloat (axisY - 10))
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
            String.fromFloat arrowX
                ++ ","
                ++ String.fromFloat tipY
                ++ " "
                ++ String.fromFloat (arrowX - halfW)
                ++ ","
                ++ String.fromFloat baseY
                ++ " "
                ++ String.fromFloat (arrowX + halfW)
                ++ ","
                ++ String.fromFloat baseY
    in
    Svg.polygon
        [ SA.points points
        , SA.fill indigoAccent
        ]
        []



-- Aspect shapes (drawn above the axis)


viewAspectShape : Float -> Bool -> Bool -> Svg.Svg msg
viewAspectShape arrowX perfect progressive =
    let
        shapeY =
            axisY - 30

        centerX =
            arrowX
    in
    case ( perfect, progressive ) of
        ( False, False ) ->
            viewSimpleDot centerX shapeY

        ( False, True ) ->
            viewProgressiveBar centerX shapeY

        ( True, False ) ->
            viewPerfectShape centerX shapeY

        ( True, True ) ->
            viewPerfectProgressiveShape centerX shapeY



-- Simple: filled circle (point event)


viewSimpleDot : Float -> Float -> Svg.Svg msg
viewSimpleDot cx cy =
    Svg.circle
        [ SA.cx (String.fromFloat cx)
        , SA.cy (String.fromFloat cy)
        , SA.r "8"
        , SA.fill emeraldFill
        ]
        []



-- Progressive: elongated rounded bar with wavy interior


viewProgressiveBar : Float -> Float -> Svg.Svg msg
viewProgressiveBar cx cy =
    let
        barW =
            60

        barH =
            16

        x =
            cx - barW / 2

        y =
            cy - barH / 2

        waveY =
            cy
    in
    Svg.g []
        [ Svg.rect
            [ SA.x (String.fromFloat x)
            , SA.y (String.fromFloat y)
            , SA.width (String.fromFloat barW)
            , SA.height (String.fromFloat barH)
            , SA.rx "8"
            , SA.fill emeraldFillAlpha
            , SA.stroke emeraldFill
            , SA.strokeWidth "1.5"
            ]
            []
        , Svg.path
            [ SA.d (wavePath (x + 8) waveY (barW - 16) 3 4)
            , SA.stroke emeraldFill
            , SA.strokeWidth "1.5"
            , SA.fill "none"
            , SA.strokeLinecap "round"
            ]
            []
        ]



-- Perfect: line ending at a filled circle


viewPerfectShape : Float -> Float -> Svg.Svg msg
viewPerfectShape cx cy =
    let
        lineLen =
            30
    in
    Svg.g []
        [ Svg.line
            [ SA.x1 (String.fromFloat (cx - lineLen))
            , SA.y1 (String.fromFloat cy)
            , SA.x2 (String.fromFloat cx)
            , SA.y2 (String.fromFloat cy)
            , SA.stroke emeraldFill
            , SA.strokeWidth "2"
            ]
            []
        , Svg.circle
            [ SA.cx (String.fromFloat cx)
            , SA.cy (String.fromFloat cy)
            , SA.r "6"
            , SA.fill emeraldFill
            ]
            []
        ]



-- Perfect Progressive: rounded bar ending at a circle


viewPerfectProgressiveShape : Float -> Float -> Svg.Svg msg
viewPerfectProgressiveShape cx cy =
    let
        barW =
            50

        barH =
            16

        barX =
            cx - barW - 4

        y =
            cy - barH / 2

        waveY =
            cy
    in
    Svg.g []
        [ Svg.rect
            [ SA.x (String.fromFloat barX)
            , SA.y (String.fromFloat y)
            , SA.width (String.fromFloat barW)
            , SA.height (String.fromFloat barH)
            , SA.rx "8"
            , SA.fill emeraldFillAlpha
            , SA.stroke emeraldFill
            , SA.strokeWidth "1.5"
            ]
            []
        , Svg.path
            [ SA.d (wavePath (barX + 8) waveY (barW - 16) 3 4)
            , SA.stroke emeraldFill
            , SA.strokeWidth "1.5"
            , SA.fill "none"
            , SA.strokeLinecap "round"
            ]
            []
        , Svg.circle
            [ SA.cx (String.fromFloat (cx + 4))
            , SA.cy (String.fromFloat cy)
            , SA.r "6"
            , SA.fill emeraldFill
            ]
            []
        , Svg.line
            [ SA.x1 (String.fromFloat (barX + barW))
            , SA.y1 (String.fromFloat cy)
            , SA.x2 (String.fromFloat (cx + 4 - 6))
            , SA.y2 (String.fromFloat cy)
            , SA.stroke emeraldFill
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



-- Feature badges


viewBadges : Float -> Voice -> Polarity -> Maybe Modality -> Svg.Svg msg
viewBadges arrowX voice polarity modal =
    let
        badgeY =
            axisY + 40

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
                viewBadge (startX + toFloat i * spacing) badgeY label color
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
            [ SA.x (String.fromFloat rectX)
            , SA.y (String.fromFloat rectY)
            , SA.width (String.fromFloat textW)
            , SA.height "20"
            , SA.rx "10"
            , SA.fill "rgba(0, 0, 0, 0.3)"
            , SA.stroke color
            , SA.strokeWidth "1"
            ]
            []
        , Svg.text_
            [ SA.x (String.fromFloat cx)
            , SA.y (String.fromFloat (cy + 4))
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
