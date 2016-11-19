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
        active =
            "crosspadSettingsButtonActive"

        inactive =
            "crosspadSettingsButtonInactive"

        c =
            if model.symmetry == sym then
                active
            else
                inactive

        css =
            "pure-button " ++ c
    in
        button [ class css, onClick (SetSymmetry sym) ]
            [ text value ]


grid_settings : Model -> Html Msg
grid_settings model =
    let
        btn =
            setting model
    in
        div [ style [ ( "display", "inline" ) ] ]
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

        active =
            "crosspadSettingsButtonActive"

        css =
            "pure-button " ++ active
    in
        button [ class css, onClick ToggleDirection ]
            [ text dir ]


status_bar : Model -> Html Msg
status_bar model =
    div [ style [ ( "display", "inline" ) ] ]
        [ label [ class "pure-label", style [ ( "margin-left", "4px" ) ] ] [ text "Direction: " ]
        , dir_button model
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
            Html.a [ class "pure-button crosspadToolbarButton", onClick action ]
                [ text txt ]
    in
        div [ style [ ( "display", "inline" ) ] ]
            [ Html.form [ class "pure-form", id "convert-form" ]
                [ fieldset []
                    [ text "Load as:"
                    , load_format_selector model
                    , input [ id "file-upload", type_ "file", class "pure-button crosspadToolbarButton" ] []
                    , btn "Load" UploadFile
                    ]
                , fieldset []
                    [ text "Convert to:"
                    , save_format_selector model
                    , text "Filename:"
                    , input
                        [ id "file-download"
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
            textarea [ cols 65, rows 12, readonly True, value str ] []

        lbl str =
            label [ style [ ( "display", "block" ) ] ] [ text str ]
    in
        div []
            [ lbl "Across"
            , txt ac
            , lbl "Down"
            , txt dn
            ]



-- GRID --


grid : Model -> Html Msg
grid model =
    let
        s =
            if model.grid_active then
                [ ( "display", "inline-block" )
                , ( "border-style", "solid" )
                , ( "border-color", "#FF0000" )
                , ( "border-width", "2px" )
                , ( "padding", "2px" )
                , ( "margin", "2px" )
                ]
            else
                [ ( "display", "inline-block" )
                , ( "border-color", "#FFFFFF" )
                , ( "border-style", "solid" )
                , ( "border-width", "2px" )
                , ( "padding", "2px" )
                , ( "margin", "2px" )
                ]
    in
        div
            [ onBlur GridLostFocus
            , onFocus GridGainedFocus
            , tabindex -1
            , style s
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

        set_sym =
            grid_settings model

        tb =
            toolbar model

        row =
            div [ class "pure-g" ]
    in
        div []
            [ row [ div [ class "pure-u-2-3 crosspadStatusBarContainer" ] [ tb ] ]
            , row
                [ div [ class "pure-u-1-3 crosspadGridContainer" ] [ g ]
                , div [ class "pure-u-1-3 crosspadGridContainer" ] [ c ]
                ]
            , row
                [ div [ class "pure-u-2-3 crosspadStatusBarContainer" ] [ set_sym, sb ]
                ]
            ]
