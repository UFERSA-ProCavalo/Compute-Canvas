module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import TuringMachine exposing (TuringModel, Msg(..), init, update, view)

-- Modelo
type alias Model =
    { selectedMenuItem : MenuItem
    , turingModel : TuringModel
    , isTableOpen : Bool
    }

type MenuItem
    = TuringMachine
    | PostMachine
    | StackMachine
    | TwoStackAutomaton

-- Mensagens
type Msg
    = SelectMenuItem MenuItem
    | TuringMsg TuringMachine.Msg
    | ToggleTable
    | ResetTuringMachine

-- Inicialização
init : Model
init =
    { selectedMenuItem = TuringMachine
    , turingModel = TuringMachine.init
    , isTableOpen = False
    }

-- Atualização
update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectMenuItem item ->
            { model | selectedMenuItem = item }

        TuringMsg turingMsg ->
            let
                updatedModel = TuringMachine.update turingMsg model.turingModel
            in
            { model | turingModel = updatedModel }

        ToggleTable ->
            { model | isTableOpen = not model.isTableOpen }

        ResetTuringMachine ->
            { model | turingModel = TuringMachine.init }

-- Visualização
view : Model -> Html Msg
view model =
    div [ style "display" "flex", style "height" "100vh", style "box-sizing" "border-box" ]
        [ div [ style "width" "20%", style "box-sizing" "border-box" ]
            [ menuView model.selectedMenuItem ]
        , div [ style "width" "60%", style "padding" "10px", style "box-sizing" "border-box" ]
            [ mainView model ]
        , div [ style "width" "20%", style "border-left" "1px solid black", style "display" (if model.isTableOpen then "block" else "none"), style "box-sizing" "border-box" ]
            [ tableView model.selectedMenuItem model.turingModel.transitions ]
        , div [ style "position" "absolute", style "bottom" "0", style "width" "100%", style "border-top" "1px solid black", style "padding" "10px", style "box-sizing" "border-box" ]
            [ inputView model.selectedMenuItem model.turingModel.inputString ]
        ]

menuView : MenuItem -> Html Msg
menuView selected =
    div []
        [ button [ onClick (SelectMenuItem TuringMachine) ] [ text "Máquina de Turing" ]
        , button [ onClick (SelectMenuItem PostMachine) ] [ text "Máquina de Post" ]
        , button [ onClick (SelectMenuItem StackMachine) ] [ text "Máquina com pilhas" ]
        , button [ onClick (SelectMenuItem TwoStackAutomaton) ] [ text "Autômato com duas pilhas" ]
        ]

mainView : Model -> Html Msg
mainView model =
    case model.selectedMenuItem of
        TuringMachine ->
            div []
                [ Html.map TuringMsg (TuringMachine.view model.turingModel)
                ]

        _ ->
            div [] [ text "Visualização do modelo selecionado" ]

tableView : MenuItem -> List TuringMachine.Transition -> Html Msg
tableView selected transitions =
    case selected of
        TuringMachine ->
            div []
                [ TuringMachine.transitionTable transitions ]

        _ ->
            div []
                [ text ("Tabela do modelo: " ++ (menuItemToString selected)) ]

inputView : MenuItem -> String -> Html Msg
inputView selected userInput =
    case selected of
        TuringMachine ->
            div []
                [ input [ placeholder "Digite a string de entrada", value userInput, onInput (TuringMsg << UpdateInputString) ] []
                , button [ onClick (TuringMsg ProcessNext) ] [ text "Processar Próximo" ]
                , button [ onClick ToggleTable ] [ text "Abrir/Fechar Tabela" ]
                , button [ onClick ResetTuringMachine ] [ text "Resetar Máquina" ]
                ]

        _ ->
            div []
                [ input [ placeholder "Digite a linguagem do modelo", value userInput, onInput (\_ -> SelectMenuItem selected) ] []
                , button [ onClick ToggleTable ] [ text "Abrir/Fechar Tabela" ]
                ]

menuItemToString : MenuItem -> String
menuItemToString item =
    case item of
        TuringMachine ->
            "Máquina de Turing"

        PostMachine ->
            "Máquina de Post"

        StackMachine ->
            "Máquina com pilhas"

        TwoStackAutomaton ->
            "Autômato com duas pilhas"

-- Programa Principal
main =
    Browser.sandbox { init = init, update = update, view = view }