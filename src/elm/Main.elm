module Main exposing (..)

import Html exposing (Html, div, span, form, fieldset, input, label)
import Html.App as App
import Html.Attributes as Html exposing (name, type', id)
import Html.Events exposing (onClick)
import Keyboard exposing (..)
import Svg.Attributes as Svg exposing (version, height, width, x, y, r, cx, cy, style)
import Svg exposing (Svg, circle, rect, svg, g, line, text, text')

import Array2D
import Char exposing (..)
import String exposing (..)

import Keycode exposing (..)
import Xword exposing (..)
import Html.CssHelpers
import MyCss

grid = Array2D.repeat 15 15 empty_square

xword =
  { rows = 15, cols = 15, grid = grid }
  |> renumber

type Symmetry = SymmNone | Symm180 | Symm90 | GridLocked

-- MODEL --
type alias Model =
  { width : Int
  , height : Int
  , xw : Xword
  , cur_x : Int
  , cur_y : Int
  , cur_dir : Direction
  , last_key : String
  , symmetry : Symmetry
  }

init_model : Model
init_model =
    { width = 15
    , height = 15
    , xw = xword
    , cur_x = 0
    , cur_y = 0
    , cur_dir = Across
    , last_key = "None"
    , symmetry = Symm180
    }


init : (Model, Cmd Msg)
init = (init_model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.presses (\code -> KeyPress (fromCode code))
    , Keyboard.downs (\code -> KeyDown (fromKeyCode code))
    ]


-- UPDATE --

type Msg =
  ClickSquare Int Int
  | KeyPress Char
  | KeyDown Key
  | SetSymmetry Symmetry

is_current_black model =
  is_black model.cur_x model.cur_y model.xw.grid

set_current_square : Square -> Model -> Model
set_current_square sq model =
  let x = model.cur_x
      y = model.cur_y
      xw = model.xw
      g = set_square x y sq xw.grid
      xw' = {xw | grid = g }
  in
      { model | xw = xw' }

update_letter : Char -> Model -> Model
update_letter c model =
  let s = String.fromChar c |> String.toUpper
      sq = letter_square s
  in
      if is_current_black model then
        model
      else
        set_current_square sq model

move_cursor dx dy model =
  let x = (model.cur_x + dx) % model.width
      y = (model.cur_y + dy) % model.height
  in
      { model | cur_x = x, cur_y = y }

advance_cursor model =
  case model.cur_dir of
    Across -> move_cursor 1 0 model
    Down -> move_cursor 0 1 model

retreat_cursor model =
  case model.cur_dir of
    Across -> move_cursor -1 0 model
    Down -> move_cursor 0 -1 model

toggle_black : Model -> Model
toggle_black model =
  let x = model.cur_x
      y = model.cur_y
      xw = model.xw
      sq = if is_black x y xw.grid then empty_square else black_square
      squares = case model.symmetry of
        GridLocked -> []
        SymmNone -> [(x, y)]
        Symm90 -> symm_90 x y xw
        Symm180 -> symm_180 x y xw
      grid' = List.foldl (\(i, j) grid ->
        set_square i j sq grid)
        xw.grid
        squares
      xw' = { xw | grid = grid' }
  in
      { model | xw = xw' } |> advance_cursor

update_numbers : Model -> Model
update_numbers model =
  let xw' = model.xw |> renumber
  in
      { model | xw = xw' }

toggle_dir : Model -> Model
toggle_dir model =
  let dir' = if model.cur_dir == Across then Down else Across
  in
      { model | cur_dir = dir' }

delete_current : Model -> Model
delete_current model =
  if is_current_black model then
    model
  else
    set_current_square empty_square model |> update_numbers

backspace_current : Model -> Model
backspace_current model =
  delete_current model |> retreat_cursor

handle_keycode k model =
  case k of
    ArrowLeft -> move_cursor -1 0 model
    ArrowRight -> move_cursor 1 0 model
    ArrowUp -> move_cursor 0 -1 model
    ArrowDown -> move_cursor 0 1 model
    Space -> toggle_black model |> update_numbers
    PageDown -> toggle_dir model
    Delete -> delete_current model
    Backspace -> backspace_current model
    _ -> model

handle_keypress c model =
  if Char.isUpper(c) || Char.isLower(c) || Char.isDigit(c) then
    update_letter c model |>
    update_numbers |>
    advance_cursor
  else
    model

handle_symm symm model =
  { model | symmetry = symm }

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ClickSquare x y -> ({ model | cur_x = x, cur_y = y }, Cmd.none)
    KeyDown k -> (handle_keycode k model, Cmd.none)
    KeyPress c -> (handle_keypress c model, Cmd.none)
    SetSymmetry s -> (handle_symm s model, Cmd.none)

-- VIEW --

x0 = 20
y0 = 20

top_x = toString x0
top_y = toString y0

s = 32
cw = toString s

grid_style = "fill:rgb(255,255,255);stroke-width:1;stroke:rgb(0,0,0)"
white_style = "fill:rgb(255,255,255);fill-opacity:0;stroke-width:1;stroke:rgb(0,0,0)"
black_style = "fill:rgb(0,0,0);stroke-width:1;stroke:rgb(0,0,0)"
cwhite_style = "fill:rgb(128,128,255);fill-opacity:0.5;stroke-width:1;stroke:rgb(0,0,0)"
cblack_style = "fill:rgb(0,0,128);fill-opacity:0.5;stroke-width:1;stroke:rgb(0,0,0)"
numstyle = "font-family:arial;font-size:0.625rem"
letstyle = "font-family:arial;font-size:1.0rem;text-anchor:middle"

fstyle cell is_cur =
  case (cell, is_cur) of
    (Black, False) -> black_style
    (Black, True) -> cblack_style
    (_, False) -> white_style
    (_, True) -> cwhite_style

cellstyle : Int -> Int -> Model -> String
cellstyle x y model =
  let c = Array2D.get x y model.xw.grid
      is_cur = model.cur_x == x && model.cur_y == y
  in
  case c of
    Nothing -> black_style
    Just cell -> fstyle cell.cell is_cur

cell model x' y' =
  let x0' = x0 + x' * s
      y0' = y0 + y' * s
      cell_x = x0' |> toString
      cell_y = y0' |> toString
      num_x = x0' + 1 |> toString
      num_y = y0' + round (s/3) |> toString
      let_x = x0' + round (s/2) |> toString
      let_y = y0' + s - 5 |> toString
      xw = model.xw
      grid = xw.grid
      cstyle = cellstyle x' y' model
      letter = get_letter x' y' grid
      number = get_number x' y' grid
  in
  g []
    [
      text' [ style numstyle, x num_x, y num_y, style numstyle ] [text number]
    , text' [ Svg.class "crosspadLetter", x let_x, y let_y ] [text letter]
    , rect [ x cell_x, y cell_y, width cw, height cw, style cstyle, onClick (ClickSquare x' y')  ] []]

cells model = List.concatMap (\y -> List.map (\x -> cell model x y) [0..14]) [0..14]

radio : String -> msg -> Html msg
radio value msg =
  label []
    [ input [ type' "radio", name "font-size", onClick msg ] []
    , Html.text value
    ]

svg_grid : Model -> Html Msg
svg_grid model =
  let w = model.width * s
      h = model.height * s
      w' = toString w
      h' = toString h
      svg_h = toString (h + 20) ++ "px"
      svg_w = toString (w + 20) ++ "px"
  in
      svg
        [ version "1.1", x "0", y "0", height svg_h, width svg_w ] (
          [ rect [ x top_x, y top_y, width w', height h', style grid_style ] []
          ] ++ cells model)

grid_settings : Html Msg
grid_settings =
  fieldset [Html.style [("float", "left")]]
    [ radio "None" (SetSymmetry SymmNone)
    , radio "180" (SetSymmetry Symm180)
    , radio "90" (SetSymmetry Symm90)
    , radio "Locked" (SetSymmetry GridLocked)
    ]

status_bar : Model -> Html Msg
status_bar model =
  let dir =
        case model.cur_dir of
          Across -> "Across"
          Down -> "Down"
  in
      fieldset [Html.style [("float", "left")]]
        [ label [ Html.style [("padding", "10px")] ]
            [ Html.text dir]
        ]


view model =
  let g = svg_grid model
      sb = status_bar model
  in
      div []
        [ g
        , div []
          [ grid_settings
          , sb
          ]
        ]

-- MAIN --
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
