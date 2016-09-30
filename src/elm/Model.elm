module Model exposing (..)

import Array2D
import Char exposing (..)
import String exposing (..)

import Keycode exposing (..)
import Xword exposing (..)

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

