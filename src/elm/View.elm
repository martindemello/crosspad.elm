module View exposing (view)

import Model exposing (..)
import GridView exposing (svg_grid)
import Types exposing (..)
import Utils exposing (..)
import Xword
import Html exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, onFocus)
import Html.Attributes exposing (..)
import Kintail.InputWidget as InputWidget


-- SETTINGS --


setting : Model -> String -> Symmetry -> Html Msg
setting model value sym =
    let
        c =
            if model.symmetry == sym then
                "active"
            else
                "inactive"

        css =
            "crosspad-button-" ++ c
    in
        button [ class css, onClick (SetSymmetry sym) ]
            [ text value ]


grid_settings : Model -> Html Msg
grid_settings model =
    let
        btn =
            setting model
    in
        span []
            [ label [ class "crosspad-status-label" ] [ text "Symmetry: " ]
            , btn "None" SymmNone
            , btn "180" Symm180
            , btn "90" Symm90
            , btn "Locked" GridLocked
            ]


dir_button : Model -> Html Msg
dir_button model =
    let
        dir =
            case model.cursor.dir of
                Across ->
                    "Across"

                Down ->
                    "Down"

        css =
            "crosspad-button-active"
    in
        button [ class css, onClick ToggleDirection ]
            [ text dir ]


direction_settings : Model -> Html Msg
direction_settings model =
    span [ class "crosspad-status-group" ]
        [ label [ class "crosspad-status-label" ] [ text "Direction: " ]
        , dir_button model
        ]


status_bar : Model -> Html Msg
status_bar model =
    div [ class "crosspad-statusbar" ]
        [ grid_settings model
        , direction_settings model
        ]


load_format_selector : Model -> Html Msg
load_format_selector model =
    InputWidget.comboBox [ class "crosspad-toolbar-combo" ] identity Model.load_formats model.load_format
        |> Html.map LoadFormatChanged


save_format_selector : Model -> Html Msg
save_format_selector model =
    InputWidget.comboBox [ class "crosspad-toolbar-combo" ] identity Model.save_formats model.save_format
        |> Html.map SaveFormatChanged


toolbar : Model -> Html Msg
toolbar model =
    let
        btn txt action =
            Html.a [ class "crosspad-button-toolbar", onClick action ]
                [ text txt ]
    in
        div [ style [ ( "display", "inline" ) ] ]
            [ Html.form [ id "convert-form" ]
                [ fieldset []
                    [ label [ class "crosspad-status-label" ] [ text "Load as:" ]
                    , load_format_selector model
                    , input
                        [ id "file-upload"
                        , type_ "file"
                        , class "crosspad-toolbar-input"
                        ]
                        []
                    , btn "Load" UploadFile
                    ]
                , fieldset []
                    [ label [ class "crosspad-status-label" ] [ text "Convert to:" ]
                    , save_format_selector model
                    , text "Filename:"
                    , input
                        [ id "file-download"
                        , class "crosspad-toolbar-input"
                        , onFocus GridLostFocus
                        ]
                        []
                    , btn "Download" SaveFile
                    ]
                ]
            ]



-- CLUES --


clue : Direction -> Int -> ( Int, String ) -> Html Msg
clue dir current ( n, c ) =
    let
        cls =
            when (n == current) [ class "crosspad-clue-current" ] []

        css =
            [ onClick (SetCurrentClue dir n) ] ++ cls
    in
        li css
            [ div [ class "crosspad-clue-number" ]
                [ text (toString n) ]
            , div [ class "crosspad-clue-text" ]
                [ text c ]
            ]


clue_box : Model -> Html Msg
clue_box model =
    let
        clues =
            Xword.clue_list model.xw

        list dir current cs =
            div [ class "crosspad-clue-section" ]
                [ ul [ class "crosspad-clue-list" ] (List.map (clue dir current) cs)
                ]

        lbl str =
            p [ class "crosspad-clue-label" ] [ text str ]
    in
        div [ class "crosspad-clues-container" ]
            [ lbl "Across"
            , list Across model.current_ac clues.across
            , lbl "Down"
            , list Down model.current_dn clues.down
            ]



-- GRID --


grid : Model -> Html Msg
grid model =
    let
        css =
            if model.grid_active then
                "crosspad-grid-border-active"
            else
                "crosspad-grid-border-inactive"
    in
        div
            [ onBlur GridLostFocus
            , onFocus GridGainedFocus
            , tabindex -1
            , class css
            ]
            [ svg_grid model ]



-- VIEW --


view : Model -> Html Msg
view model =
    let
        g =
            grid model

        clues =
            clue_box model

        sb =
            status_bar model

        tb =
            toolbar model
    in
        div [ id "view-main" ]
            [ div [ class "crosspad-statusbar-top" ] [ tb ]
            , div [ class "crosspad-main" ]
                [ div [ class "crosspad-grid-container" ] [ g ]
                , clues
                ]
            , div [ class "crosspad-statusbar-bottom" ] [ sb ]
            ]
