module TuringMachine exposing (..)

import Html exposing (Html, div, text, button, input, table, tr, td)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (getAt, updateAt)
import Svg exposing (..)
import Svg.Attributes exposing (..)

-- Modelo
type alias TuringModel =
    { inputString : String
    , tape : List Char
    , headPosition : Int
    , currentState : String
    , transitions : List Transition
    , errorMessage : String
    , successMessage : String
    }

type alias Transition =
    { fromState : String
    , readSymbol : Char
    , toState : String
    , writeSymbol : Char
    , moveDirection : Direction
    }

type Direction = L | R

-- Mensagens
type Msg
    = UpdateInputString String
    | ProcessNext

-- Inicialização
init : TuringModel
init =
    { inputString = ""
    , tape = []
    , headPosition = 0
    , currentState = "q0"
    , transitions = initialTransitions
    , errorMessage = ""
    , successMessage = ""
    }

initialTransitions : List Transition
initialTransitions =
    [ { fromState = "q0", readSymbol = 'a', toState = "q1", writeSymbol = 'X', moveDirection = R }
    , { fromState = "q0", readSymbol = 'Y', toState = "q3", writeSymbol = 'Y', moveDirection = R }
    , { fromState = "q0", readSymbol = '_', toState = "q4", writeSymbol = '_', moveDirection = R }
    , { fromState = "q1", readSymbol = 'a', toState = "q1", writeSymbol = 'a', moveDirection = R }
    , { fromState = "q1", readSymbol = 'b', toState = "q2", writeSymbol = 'Y', moveDirection = L }
    , { fromState = "q1", readSymbol = 'Y', toState = "q1", writeSymbol = 'Y', moveDirection = R }
    , { fromState = "q2", readSymbol = 'a', toState = "q2", writeSymbol = 'a', moveDirection = L }
    , { fromState = "q2", readSymbol = 'X', toState = "q0", writeSymbol = 'X', moveDirection = R }
    , { fromState = "q2", readSymbol = 'Y', toState = "q2", writeSymbol = 'Y', moveDirection = L }
    , { fromState = "q3", readSymbol = 'Y', toState = "q3", writeSymbol = 'Y', moveDirection = R }
    , { fromState = "q3", readSymbol = '_', toState = "q4", writeSymbol = '_', moveDirection = R }
    ]

-- Atualização
update : Msg -> TuringModel -> TuringModel
update msg model =
    case msg of
        UpdateInputString input ->
            { model | inputString = input, tape = String.toList input, errorMessage = "", successMessage = "" }

        ProcessNext ->
            if model.currentState == "q4" then
                { model | successMessage = "Cadeia aceita pela linguagem" }
            else
                let
                    currentSymbol = getAt model.headPosition model.tape |> Maybe.withDefault '_'
                    applicableTransition = List.filter (\t -> t.fromState == model.currentState && t.readSymbol == currentSymbol) model.transitions |> List.head
                in
                case applicableTransition of
                    Just transition ->
                        let
                            newTape = updateAt model.headPosition (\_ -> transition.writeSymbol) model.tape
                            newHeadPosition = case transition.moveDirection of
                                L -> model.headPosition - 1
                                R -> model.headPosition + 1
                        in
                        { model | tape = newTape, headPosition = newHeadPosition, currentState = transition.toState, errorMessage = "" }

                    Nothing ->
                        { model | errorMessage = "Cadeia não aceita pela linguagem" }

-- Visualização
view : TuringModel -> Html Msg
view model =
    div [ Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "column", Html.Attributes.style "height" "90vh" ]
        [ div [ Html.Attributes.style "flex" "1", Html.Attributes.style "overflow" "auto" ]
            [ div [] [ Html.text "Fita: ", tapeView model.tape model.headPosition ]
            , div [] [ Html.text ("Cabeçote: " ++ String.fromInt model.headPosition) ]
            , div [] [ Html.text ("Estado Atual: " ++ model.currentState) ]
            , if model.errorMessage /= "" then
                div [ Html.Attributes.style "color" "red" ] [ Html.text model.errorMessage ]
              else
                Html.text ""
            , if model.successMessage /= "" then
                div [ Html.Attributes.style "color" "green" ] [ Html.text model.successMessage ]
              else
                Html.text ""
            , graphView model
            ]
            
        ]
tapeView : List Char -> Int -> Html msg
tapeView tape headPosition =
    div []
        (List.indexedMap (\index char -> if index == headPosition then 
        Html.span [ Html.Attributes.style "background-color" "yellow" ] 
        [ Html.text (String.fromChar char) ] 
        else 
        Html.span [] [ Html.text (String.fromChar char) ]) tape)

transitionTable : List Transition -> Html msg
transitionTable transitions =
    table []
        (List.map transitionRow transitions)

transitionRow : Transition -> Html msg
transitionRow transition =
    tr []
        [ td [] [ Html.text transition.fromState ]
        , td [] [ Html.text (String.fromChar transition.readSymbol) ]
        , td [] [ Html.text transition.toState ]
        , td [] [ Html.text (String.fromChar transition.writeSymbol) ]
        , td [] [ Html.text (directionToString transition.moveDirection) ]
        ]

directionToString : Direction -> String
directionToString direction =
    case direction of
        L -> "L"
        R -> "R"

-- Visualização do Grafo
graphView : TuringModel -> Html msg
graphView model =
    let
        nodes = 
            [ { id = "q0", label = "q0", x = 100, y = 100 }
            , { id = "q1", label = "q1", x = 300, y = 100 }
            , { id = "q2", label = "q2", x = 500, y = 100 }
            , { id = "q3", label = "q3", x = 700, y = 100 }
            , { id = "q4", label = "q4", x = 900, y = 100 }
            ]
        edges = 
            [ { from = "q0", to = "q1", label = "a/X, R" }
            , { from = "q1", to = "q1", label = "a/a, R" }
            , { from = "q1", to = "q2", label = "b/Y, L" }
            , { from = "q2", to = "q2", label = "a/a, L" }
            , { from = "q2", to = "q3", label = "X/X, R" }
            , { from = "q3", to = "q3", label = "Y/Y, R" }
            , { from = "q3", to = "q4", label = "_/_, R" }
            ]
    in
    svg [ Svg.Attributes.width "100%", Svg.Attributes.height "400" ]
        (List.concatMap (edgeView nodes) edges ++ List.map (nodeView model.currentState) nodes)

nodeView : String -> { id : String, label : String, x : Float, y : Float } -> Svg msg
nodeView currentState node =
    let
        isCurrent = node.id == currentState
        fillColor = if isCurrent then "red" else "white"
    in
    g []
        [ circle [ cx (String.fromFloat node.x), cy (String.fromFloat node.y), r "20", fill fillColor, stroke "black" ] []
        , text_ [ x (String.fromFloat node.x), y (String.fromFloat (node.y + 5)), textAnchor "middle", fontSize "12" ] [ Html.text node.label ]
        ]

edgeView : List { id : String, label : String, x : Float, y : Float } -> { from : String, to : String, label : String } -> List (Svg msg)
edgeView nodes edge =
    let
        fromNode = List.head (List.filter (\n -> n.id == edge.from) nodes)
        toNode = List.head (List.filter (\n -> n.id == edge.to) nodes)
    in
    case (fromNode, toNode) of
        (Just from, Just to) ->
            [ line [ x1 (String.fromFloat from.x), y1 (String.fromFloat from.y), x2 (String.fromFloat to.x), y2 (String.fromFloat to.y), stroke "black" ] []
            , text_ [ x (String.fromFloat ((from.x + to.x) / 2)), y (String.fromFloat ((from.y + to.y) / 2 - 10)), textAnchor "middle", fontSize "12" ] [ Html.text edge.label ]
            ]

        _ ->
            []