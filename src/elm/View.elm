module View exposing (view)

import Model exposing (..)
import Xword exposing (..)
import GridView exposing (svg_grid)

import Html exposing (
  Html, div, span, form, fieldset, input, label, button, text
  )
import Html.Events exposing (onClick)
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
        case model.cur_dir of
          Across -> "Across"
          Down -> "Down"
      active = "crosspadSettingsButtonActive"
      css = "pure-button " ++ active
  in
    button [ class css, onClick ToggleDirection ]
      [text dir]

status_bar : Model -> Html Msg
status_bar model =
  let dir =
        case model.cur_dir of
          Across -> "Across"
          Down -> "Down"
  in
      div [style [("display", "inline")]]
        [ label [class "pure-label", style [("margin-left", "4px")]] [text "Direction: "]
        , dir_button model
        ]


-- VIEW --

view : Model -> Html Msg
view model =
  let g = svg_grid model
      sb = status_bar model
      set_sym = grid_settings model
  in
      div []
        [ div [class "pure-g"]
            [ div [class "crosspadGridContainer pure-u-1-2"] [g] ]
        , div [class "pure-g"]
            [ div [class "pure-u-1-2"
                  , style [("display", "inline")
                          ,("background-color", "#f0f0f0")
                          ]
                  ]
                [set_sym, sb]
            ]
        ]
