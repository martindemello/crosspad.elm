module View exposing (view)

import Model exposing (..)
import GridView exposing (svg_grid)
import Types exposing (..)
import Xword
import Html exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, onFocus)
import Html.Attributes exposing (..)
import String
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
            "pure-button crosspad-button-" ++ c
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
            [ label [ class "pure-label" ] [ text "Symmetry: " ]
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
            "pure-button crosspad-button-active"
    in
        button [ class css, onClick ToggleDirection ]
            [ text dir ]


direction_settings : Model -> Html Msg
direction_settings model =
    span [ class "crosspad-status-group" ]
        [ label [ class "pure-label crosspad-status-label" ] [ text "Direction: " ]
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
    InputWidget.comboBox [] identity Model.load_formats model.load_format
        |> Html.map LoadFormatChanged


save_format_selector : Model -> Html Msg
save_format_selector model =
    InputWidget.comboBox [] identity Model.save_formats model.save_format
        |> Html.map SaveFormatChanged


toolbar : Model -> Html Msg
toolbar model =
    let
        btn txt action =
            Html.a [ class "pure-button crosspad-button-toolbar", onClick action ]
                [ text txt ]
    in
        div [ style [ ( "display", "inline" ) ] ]
            [ Html.form [ class "pure-form", id "convert-form" ]
                [ fieldset []
                    [ text "Load as:"
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
                    [ text "Convert to:"
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


clue_box : Model -> Html Msg
clue_box model =
    let
        lines =
            String.join "\n"

        merge ( x, y ) =
            (toString x) ++ ". " ++ y

        format c =
            lines <| List.map merge c

        clues =
            Xword.clue_list model.xw

        ac =
            format clues.across

        dn =
            format clues.down

        txt str =
            textarea [ class "crosspad-clue-textbox", cols 65, rows 12, readonly True, value str ] []

        lbl str =
            label [ class "crosspad-clue-label" ] [ text str ]
    in
        div [ class "crosspad-clue-textbox" ]
            [ lbl "Across"
            , txt ac
            , lbl "Down"
            , txt dn
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

        c =
            clue_box model

        sb =
            status_bar model

        tb =
            toolbar model

        row =
            div [ class "pure-g" ]
    in
        div []
            [ row [ div [ class "pure-u-1 crosspad-statusbar-container" ] [ tb ] ]
            , div [ style [ ( "display", "inline-block" ) ] ]
                [ span [ class "crosspad-grid-container" ] [ g ]
                , span [ class "crosspad-clue-container" ] [ c ]
                ]
            , row
                [ div [ class "pure-u-1 crosspad-statusbar-container" ] [ sb ]
                ]
            ]
