module Timeline exposing (TimelineMode(..), view)

import Animator
import Grammar.Types exposing (Modality(..), Polarity(..), Tense(..), VerbTense(..), Voice(..), specToVerbTense, verbTenseLabel, verbTenseToSpec)
import Html exposing (Html)
import Html.Attributes
import Svg exposing (svg)
import Svg.Attributes as SA
import Svg.Events as SE


type TimelineMode
    = CalculatorDisplay
    | ExerciseBlank
    | ExerciseFeedback { correct : VerbTense, userAnswer : Maybe VerbTense }



-- Config


type alias Config msg =
    { tenseTimeline : Animator.Timeline Tense
    , tense : Tense
    , perfect : Bool
    , progressive : Bool
    , voice : Voice
    , polarity : Polarity
    , modal : Maybe Modality
    , onSelectVerbTense : VerbTense -> msg
    , mode : TimelineMode
    }



-- Coordinates


leftEdge : Float
leftEdge =
    pastX - (presentX - pastX)


pastX : Float
pastX =
    200


presentX : Float
presentX =
    380


futureX : Float
futureX =
    560


rightEdge : Float
rightEdge =
    futureX + (presentX - pastX)


halfwayAnchorX : Float
halfwayAnchorX =
    120


axisY : Float
axisY =
    150


circleRadius : Float
circleRadius =
    30


triangleHeight : Float
triangleHeight =
    130


arrowDepth : Float
arrowDepth =
    110


vArmHeight : Float
vArmHeight =
    120


vArmSpread : Float
vArmSpread =
    40


perfectArrowDepth : Float
perfectArrowDepth =
    150


dropLineDepth : Float
dropLineDepth =
    150


shapesBottom : Float
shapesBottom =
    axisY + perfectArrowDepth


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


brickRed : String
brickRed =
    "#9B4F4F"


dustyRose : String
dustyRose =
    "#C9A0A0"


goldenOrange : String
goldenOrange =
    "#C8A050"


peach : String
peach =
    "#E0B080"


sageGreen : String
sageGreen =
    "#8B9F6A"


slateBlue : String
slateBlue =
    "#8A9EB0"


axisColor : String
axisColor =
    "#94a3b8"


labelColor : String
labelColor =
    "#94a3b8"


pointerColor : String
pointerColor =
    "#818cf8"


circleBgColor : String
circleBgColor =
    "#0f172a"


tenseColor : VerbTense -> String
tenseColor vt =
    case vt of
        SimplePast ->
            brickRed

        PastContinuous ->
            brickRed

        PastPerfect ->
            dustyRose

        PastPerfectContinuous ->
            dustyRose

        SimplePresent ->
            goldenOrange

        PresentContinuous ->
            goldenOrange

        PresentPerfect ->
            peach

        PresentPerfectContinuous ->
            peach

        SimpleFuture ->
            sageGreen

        FutureContinuous ->
            sageGreen

        FuturePerfect ->
            slateBlue

        FuturePerfectContinuous ->
            slateBlue



-- Shape types


type TenseShape
    = SimpleArrowShape
    | VShapeShape
    | DiagonalArrowShape
    | FilledTriangleShape


tenseToShape : VerbTense -> TenseShape
tenseToShape vt =
    case vt of
        SimplePast ->
            SimpleArrowShape

        SimplePresent ->
            SimpleArrowShape

        SimpleFuture ->
            SimpleArrowShape

        PastContinuous ->
            VShapeShape

        PresentContinuous ->
            VShapeShape

        FutureContinuous ->
            VShapeShape

        PastPerfect ->
            DiagonalArrowShape

        PresentPerfect ->
            DiagonalArrowShape

        FuturePerfect ->
            DiagonalArrowShape

        PastPerfectContinuous ->
            FilledTriangleShape

        PresentPerfectContinuous ->
            FilledTriangleShape

        FuturePerfectContinuous ->
            FilledTriangleShape



-- Shape coordinate helpers


diagonalAnchorAndTarget : VerbTense -> ( Float, Float )
diagonalAnchorAndTarget vt =
    case vt of
        PastPerfect ->
            ( pastX - (presentX - pastX), pastX )

        PresentPerfect ->
            ( pastX, presentX )

        FuturePerfect ->
            ( presentX, futureX )

        _ ->
            ( 0, 0 )


triangleLeftAndRight : VerbTense -> ( Float, Float )
triangleLeftAndRight vt =
    case vt of
        PastPerfectContinuous ->
            ( pastX - (presentX - pastX), pastX )

        PresentPerfectContinuous ->
            ( pastX - (presentX - pastX), presentX )

        FuturePerfectContinuous ->
            ( pastX - (presentX - pastX), futureX )

        _ ->
            ( 0, 0 )



-- Helpers


ff : Float -> String
ff =
    String.fromFloat


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



-- Circle perimeter intersection: point on circle at (centerX, centerY) facing (fromX, fromY)


circleEdge : Float -> Float -> Float -> Float -> ( Float, Float )
circleEdge fromX fromY centerX centerY =
    let
        dx =
            fromX - centerX

        dy =
            fromY - centerY

        dist =
            sqrt (dx * dx + dy * dy)
    in
    if dist == 0 then
        ( centerX, centerY + circleRadius )

    else
        ( centerX + circleRadius * dx / dist
        , centerY + circleRadius * dy / dist
        )



-- Arrowhead helper (atan2-based triangle at tip of any line)


arrowheadPoints : Float -> Float -> Float -> Float -> Float -> String
arrowheadPoints tipX tipY fromX fromY size =
    let
        angle =
            atan2 (tipY - fromY) (tipX - fromX)

        leftAngle =
            angle + pi * 5 / 6

        rightAngle =
            angle - pi * 5 / 6

        lx =
            tipX + size * cos leftAngle

        ly =
            tipY + size * sin leftAngle

        rx =
            tipX + size * cos rightAngle

        ry =
            tipY + size * sin rightAngle
    in
    ff tipX ++ "," ++ ff tipY ++ " " ++ ff lx ++ "," ++ ff ly ++ " " ++ ff rx ++ "," ++ ff ry



-- Shape renderers
-- Simple arrows, V-shapes, and diagonal arrows render BELOW the axis, pointing UP to circle perimeters.
-- Triangles render ABOVE the axis (unchanged).


viewSimpleArrow : Float -> String -> Svg.Svg msg
viewSimpleArrow x color =
    let
        halfW =
            6

        tipY =
            axisY + circleRadius

        arrowheadBase =
            tipY + 10

        shaftBottom =
            axisY + arrowDepth
    in
    Svg.g []
        [ Svg.line
            [ SA.x1 (ff x)
            , SA.y1 (ff shaftBottom)
            , SA.x2 (ff x)
            , SA.y2 (ff arrowheadBase)
            , SA.stroke color
            , SA.strokeWidth "2.5"
            ]
            []
        , Svg.polygon
            [ SA.points
                (ff x
                    ++ ","
                    ++ ff tipY
                    ++ " "
                    ++ ff (x - halfW)
                    ++ ","
                    ++ ff arrowheadBase
                    ++ " "
                    ++ ff (x + halfW)
                    ++ ","
                    ++ ff arrowheadBase
                )
            , SA.fill color
            ]
            []
        ]


viewVShape : Float -> String -> Svg.Svg msg
viewVShape x color =
    let
        vertexY =
            axisY - circleRadius

        leftArmX =
            x - vArmSpread

        leftArmY =
            axisY - vArmHeight

        rightArmX =
            x + vArmSpread

        rightArmY =
            axisY - vArmHeight
    in
    Svg.g []
        [ Svg.line
            [ SA.x1 (ff leftArmX)
            , SA.y1 (ff leftArmY)
            , SA.x2 (ff x)
            , SA.y2 (ff vertexY)
            , SA.stroke color
            , SA.strokeWidth "2.5"
            , SA.fill "none"
            ]
            []
        , Svg.line
            [ SA.x1 (ff rightArmX)
            , SA.y1 (ff rightArmY)
            , SA.x2 (ff x)
            , SA.y2 (ff vertexY)
            , SA.stroke color
            , SA.strokeWidth "2.5"
            , SA.fill "none"
            ]
            []
        ]


viewDiagonalArrow : Float -> Float -> String -> Svg.Svg msg
viewDiagonalArrow anchorX targetX color =
    let
        startX =
            anchorX

        startY =
            axisY + perfectArrowDepth

        ( edgeX, edgeY ) =
            circleEdge startX startY targetX axisY

        gradientId =
            "arrow-fade-" ++ ff anchorX ++ "-" ++ ff targetX
    in
    Svg.g []
        [ Svg.node "defs"
            []
            [ Svg.node "linearGradient"
                [ SA.id gradientId
                , SA.gradientUnits "userSpaceOnUse"
                , SA.x1 (ff startX)
                , SA.y1 (ff startY)
                , SA.x2 (ff edgeX)
                , SA.y2 (ff edgeY)
                ]
                [ Svg.node "stop"
                    [ SA.offset "0%"
                    , SA.stopColor color
                    , SA.stopOpacity "0"
                    ]
                    []
                , Svg.node "stop"
                    [ SA.offset "30%"
                    , SA.stopColor color
                    , SA.stopOpacity "0"
                    ]
                    []
                , Svg.node "stop"
                    [ SA.offset "90%"
                    , SA.stopColor color
                    , SA.stopOpacity "1"
                    ]
                    []
                , Svg.node "stop"
                    [ SA.offset "100%"
                    , SA.stopColor color
                    , SA.stopOpacity "1"
                    ]
                    []
                ]
            ]
        , Svg.line
            [ SA.x1 (ff startX)
            , SA.y1 (ff startY)
            , SA.x2 (ff edgeX)
            , SA.y2 (ff edgeY)
            , SA.stroke ("url(#" ++ gradientId ++ ")")
            , SA.strokeWidth "2.5"
            ]
            []
        , Svg.polygon
            [ SA.points (arrowheadPoints edgeX edgeY startX startY 10)
            , SA.fill color
            ]
            []
        ]


viewDropLine : Float -> Bool -> Svg.Svg msg
viewDropLine anchorX onCircle =
    let
        startY =
            if onCircle then
                axisY + circleRadius

            else
                axisY
    in
    Svg.line
        [ SA.x1 (ff anchorX)
        , SA.y1 (ff startY)
        , SA.x2 (ff anchorX)
        , SA.y2 (ff (axisY + dropLineDepth))
        , SA.stroke "#000000"
        , SA.strokeWidth "1"
        ]
        []


viewFilledTriangle : Float -> Float -> String -> Svg.Svg msg
viewFilledTriangle leftX rightX color =
    Svg.polygon
        [ SA.points
            (ff leftX
                ++ ","
                ++ ff axisY
                ++ " "
                ++ ff leftX
                ++ ","
                ++ ff (axisY - triangleHeight)
                ++ " "
                ++ ff rightX
                ++ ","
                ++ ff axisY
            )
        , SA.fill color
        , SA.fillOpacity "0.3"
        , SA.stroke color
        , SA.strokeWidth "1.5"
        ]
        []



-- Hitbox renderers


hitboxSimpleArrow : Float -> VerbTense -> (VerbTense -> msg) -> Svg.Svg msg
hitboxSimpleArrow x vt onSelect =
    Svg.rect
        [ SA.x (ff (x - 12))
        , SA.y (ff (axisY + circleRadius))
        , SA.width "24"
        , SA.height (ff (arrowDepth - circleRadius))
        , SA.fill "transparent"
        , SA.cursor "pointer"
        , SE.onClick (onSelect vt)
        ]
        []


hitboxVShape : Float -> VerbTense -> (VerbTense -> msg) -> Svg.Svg msg
hitboxVShape x vt onSelect =
    Svg.polygon
        [ SA.points
            (ff (x - vArmSpread)
                ++ ","
                ++ ff (axisY - vArmHeight)
                ++ " "
                ++ ff x
                ++ ","
                ++ ff (axisY - circleRadius)
                ++ " "
                ++ ff (x + vArmSpread)
                ++ ","
                ++ ff (axisY - vArmHeight)
                ++ " "
                ++ ff (x + vArmSpread)
                ++ ","
                ++ ff (axisY - circleRadius)
                ++ " "
                ++ ff (x - vArmSpread)
                ++ ","
                ++ ff (axisY - circleRadius)
            )
        , SA.fill "transparent"
        , SA.cursor "pointer"
        , SE.onClick (onSelect vt)
        ]
        []


hitboxDiagonalArrow : Float -> Float -> VerbTense -> (VerbTense -> msg) -> Svg.Svg msg
hitboxDiagonalArrow anchorX targetX vt onSelect =
    Svg.line
        [ SA.x1 (ff anchorX)
        , SA.y1 (ff (axisY + perfectArrowDepth))
        , SA.x2 (ff targetX)
        , SA.y2 (ff axisY)
        , SA.stroke "transparent"
        , SA.strokeWidth "24"
        , SA.cursor "pointer"
        , SE.onClick (onSelect vt)
        ]
        []


hitboxFilledTriangle : Float -> Float -> VerbTense -> (VerbTense -> msg) -> Svg.Svg msg
hitboxFilledTriangle leftX rightX vt onSelect =
    Svg.polygon
        [ SA.points
            (ff leftX
                ++ ","
                ++ ff axisY
                ++ " "
                ++ ff leftX
                ++ ","
                ++ ff (axisY - triangleHeight)
                ++ " "
                ++ ff rightX
                ++ ","
                ++ ff axisY
            )
        , SA.fill "transparent"
        , SA.cursor "pointer"
        , SE.onClick (onSelect vt)
        ]
        []



-- CSS hover styles


viewStyles : Svg.Svg msg
viewStyles =
    Svg.node "style"
        []
        [ Svg.text """.tense-shape {
    opacity: 0.35;
    cursor: pointer;
    transition: opacity 0.2s ease;
}
.tense-shape .tense-visual {
    transition: filter 0.2s ease;
}
@keyframes tense-pulse {
  0%, 100% { opacity: 0.7; }
  50%      { opacity: 1; }
}
.tense-shape.tense-selected {
    opacity: 1;
    transition: none;
    animation: tense-pulse 1.5s ease-in-out infinite;
}
.tense-shape.tense-selected .tense-visual {
    filter: brightness(1.3) drop-shadow(0 0 10px rgba(255,255,255,0.9));
    stroke-width: 4;
}
.tense-shape:hover {
    opacity: 1;
}
.tense-shape:hover .tense-visual {
    filter: brightness(1.3) drop-shadow(0 0 6px rgba(255,255,255,0.3));
}
.tense-shape.tense-selected:hover .tense-visual {
    filter: brightness(1.2) drop-shadow(0 0 8px rgba(255,255,255,0.5));
}
@keyframes tense-correct-pulse {
  0%, 100% { opacity: 0.7; }
  50%      { opacity: 1; }
}
.tense-shape.tense-correct {
    opacity: 1;
    transition: none;
    animation: tense-correct-pulse 1.5s ease-in-out infinite;
}
.tense-shape.tense-correct .tense-visual {
    filter: brightness(1.2) drop-shadow(0 0 10px rgba(34,197,94,0.7));
    stroke-width: 4;
}
.tense-shape.tense-wrong {
    opacity: 0.8;
}
""" ]



-- All shapes (each as a hoverable group with visual + hitbox)


viewAllShapes : TimelineMode -> VerbTense -> (VerbTense -> msg) -> Svg.Svg msg
viewAllShapes mode selectedVT onSelect =
    let
        renderOrder =
            [ FuturePerfectContinuous
            , PresentPerfectContinuous
            , PastPerfectContinuous
            , PastPerfect
            , PresentPerfect
            , FuturePerfect
            , PastContinuous
            , PresentContinuous
            , FutureContinuous
            , SimplePast
            , SimplePresent
            , SimpleFuture
            ]
    in
    Svg.g []
        (List.map (\vt -> viewShapeGroup mode vt (vt == selectedVT) onSelect) renderOrder)


shapeClassName : TimelineMode -> VerbTense -> Bool -> String
shapeClassName mode vt isSelected =
    case mode of
        CalculatorDisplay ->
            if isSelected then
                "tense-shape tense-selected"

            else
                "tense-shape"

        ExerciseBlank ->
            "tense-shape"

        ExerciseFeedback { correct, userAnswer } ->
            if vt == correct then
                "tense-shape tense-correct"

            else
                case userAnswer of
                    Just ans ->
                        if vt == ans then
                            "tense-shape tense-wrong"

                        else
                            "tense-shape"

                    Nothing ->
                        "tense-shape"


viewShapeGroup : TimelineMode -> VerbTense -> Bool -> (VerbTense -> msg) -> Svg.Svg msg
viewShapeGroup mode vt isSelected onSelect =
    let
        className =
            shapeClassName mode vt isSelected

        spec =
            verbTenseToSpec vt

        x =
            tenseToX spec.tense

        color =
            tenseColor vt
    in
    Svg.g [ SA.class className ]
        (case tenseToShape vt of
            SimpleArrowShape ->
                [ Svg.g [ SA.class "tense-visual" ] [ viewSimpleArrow x color ]
                , hitboxSimpleArrow x vt onSelect
                ]

            VShapeShape ->
                [ Svg.g [ SA.class "tense-visual" ] [ viewVShape x color ]
                , hitboxVShape x vt onSelect
                ]

            DiagonalArrowShape ->
                let
                    ( anchorX, targetX ) =
                        diagonalAnchorAndTarget vt

                    onCircle =
                        anchorX == pastX || anchorX == presentX || anchorX == futureX
                in
                [ viewDropLine anchorX onCircle
                , Svg.g [ SA.class "tense-visual" ] [ viewDiagonalArrow anchorX targetX color ]
                , hitboxDiagonalArrow anchorX targetX vt onSelect
                ]

            FilledTriangleShape ->
                let
                    ( leftX, rightX ) =
                        triangleLeftAndRight vt
                in
                [ Svg.g [ SA.class "tense-visual" ] [ viewFilledTriangle leftX rightX color ]
                , hitboxFilledTriangle leftX rightX vt onSelect
                ]
        )



-- Layer 6: Axis line + tense circles with labels


viewAxis : Svg.Svg msg
viewAxis =
    Svg.line
        [ SA.x1 (ff leftEdge)
        , SA.y1 (ff axisY)
        , SA.x2 (ff rightEdge)
        , SA.y2 (ff axisY)
        , SA.stroke axisColor
        , SA.strokeWidth "2"
        , SA.pointerEvents "none"
        ]
        []


viewTenseCircles : Svg.Svg msg
viewTenseCircles =
    Svg.g []
        [ viewTenseCircle pastX "past"
        , viewTenseCircle presentX "present"
        , viewTenseCircle futureX "future"
        ]


viewTenseCircle : Float -> String -> Svg.Svg msg
viewTenseCircle x label =
    Svg.g [ SA.pointerEvents "none" ]
        [ Svg.circle
            [ SA.cx (ff x)
            , SA.cy (ff axisY)
            , SA.r (ff circleRadius)
            , SA.fill circleBgColor
            , SA.stroke axisColor
            , SA.strokeWidth "2"
            ]
            []
        , Svg.text_
            [ SA.x (ff x)
            , SA.y (ff (axisY + 4))
            , SA.textAnchor "middle"
            , SA.fill labelColor
            , SA.fontSize "10"
            , SA.fontWeight "600"
            , SA.fontFamily "system-ui, sans-serif"
            ]
            [ Svg.text label ]
        ]



-- Layer 7: Animated arrow pointer + tense name label + badges (below shapes)


viewArrow : Float -> Svg.Svg msg
viewArrow arrowXPos =
    let
        tipY =
            shapesBottom + 14

        baseY =
            shapesBottom + 28

        halfW =
            8

        points =
            ff arrowXPos
                ++ ","
                ++ ff tipY
                ++ " "
                ++ ff (arrowXPos - halfW)
                ++ ","
                ++ ff baseY
                ++ " "
                ++ ff (arrowXPos + halfW)
                ++ ","
                ++ ff baseY
    in
    Svg.polygon
        [ SA.points points
        , SA.fill pointerColor
        ]
        []


viewTenseNameLabel : Float -> VerbTense -> Svg.Svg msg
viewTenseNameLabel arrowXPos selectedVT =
    Svg.text_
        [ SA.x (ff arrowXPos)
        , SA.y (ff (shapesBottom + 50))
        , SA.textAnchor "middle"
        , SA.fill pointerColor
        , SA.fontSize "16"
        , SA.fontWeight "700"
        , SA.fontFamily "system-ui, sans-serif"
        ]
        [ Svg.text (verbTenseLabel selectedVT) ]


viewBadges : Float -> Voice -> Polarity -> Maybe Modality -> Svg.Svg msg
viewBadges arrowXPos voice polarity modal =
    let
        badgeYpos =
            shapesBottom + 70

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
            arrowXPos - toFloat (totalBadges - 1) * spacing / 2
    in
    Svg.g []
        (List.indexedMap
            (\i ( label, color ) ->
                viewBadge (startX + toFloat i * spacing) badgeYpos label color
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



-- Main view


view : Config msg -> Html msg
view config =
    let
        arrowXPos =
            Animator.move config.tenseTimeline
                (\tense ->
                    Animator.at (tenseToX tense)
                )

        selectedVT =
            specToVerbTense config.tense config.perfect config.progressive

        showPointer =
            case config.mode of
                CalculatorDisplay ->
                    True

                ExerciseFeedback _ ->
                    True

                ExerciseBlank ->
                    False
    in
    Html.div [ Html.Attributes.class "w-full flex justify-center" ]
        [ svg
            [ SA.viewBox "0 0 760 395"
            , SA.width "760"
            , SA.class "max-w-full h-auto"
            , SA.style "user-select: none"
            ]
            ([ viewStyles

             -- All shapes (hoverable groups with visual + hitbox)
             , viewAllShapes config.mode selectedVT config.onSelectVerbTense

             -- Axis + circles (pointer-events: none, drawn on top visually)
             , viewAxis
             , viewTenseCircles
             ]
                ++ (if showPointer then
                        [ viewArrow arrowXPos
                        , viewTenseNameLabel arrowXPos selectedVT
                        , viewBadges arrowXPos config.voice config.polarity config.modal
                        ]

                    else
                        []
                   )
            )
        ]
