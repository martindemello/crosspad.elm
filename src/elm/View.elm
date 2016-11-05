module View exposing (view)

import Model exposing (..)
import GridView exposing (svg_grid)
import Types exposing (..)

import Html exposing (
  Html, div, span, form, fieldset, input, label, button, text, hr
  )
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (name, style, class, type', id)

-- SETTINGS --

setting : Model -> String -> Symmetry -> Html Msg
setting model value sym =
  let active = "crosspadSettingsButtonActive"
      inactive = "crosspadSettingsButtonInactive"
      c = if model.symmetry == sym then active else inactive 
      css = "pure-button " ++ c
  in
    button [ class css, onClick (SetSymmetry sym) ]
      [text value]

grid_settings : Model -> Html Msg
grid_settings model =
  let btn = setting model
  in
   div [style [("display", "inline")]]
      [ label [class "pure-label"] [text "Symmetry: "]
      , btn "None" SymmNone
      , btn "180" Symm180
      , btn "90" Symm90
      , btn "Locked" GridLocked
      ]

dir_button : Model -> Html Msg
dir_button model =
  let dir =
        case model.cursor.dir of
          Across -> "Across"
          Down -> "Down"
      active = "crosspadSettingsButtonActive"
      css = "pure-button " ++ active
  in
    button [ class css, onClick ToggleDirection ]
      [text dir]

status_bar : Model -> Html Msg
status_bar model =
  div [style [("display", "inline")]]
    [ label [class "pure-label", style [("margin-left", "4px")]] [text "Direction: "]
    , dir_button model
    ]

toolbar : Model -> Html Msg
toolbar model =
  let btn txt action =
        button [class "pure-button crosspadToolbarButton", onClick action]
        [text txt]
  in
  div [style [("display", "inline")]]
    [
      form [class "pure-form", id "convert-form"] [
        fieldset []
          [ input [ type' "file" , class "pure-button crosspadToolbarButton"] []
          , btn "Load" UploadFile
          , div [class "pure-u-1-24"] []
          , btn "Save As" SaveFile
          ]
      ]
    ]

-- VIEW --

view : Model -> Html Msg
view model =
  let g = svg_grid model
      sb = status_bar model
      set_sym = grid_settings model
      tb = toolbar model
      row = div [class "pure-g"]
  in
      div []
        [ row [ div [class "pure-u-1-2 crosspadStatusBarContainer"] [tb] ]
        , row [ div [class "pure-u-1-2 crosspadGridContainer"] [g] ]
        , row
            [ div [class "pure-u-1-2 crosspadStatusBarContainer"] [set_sym, sb]
            ]
        ]
