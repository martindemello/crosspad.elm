module Cursor exposing (..)

import Types exposing (..)

type alias Cursor =
  { x : Int
  , y : Int
  , dir : Direction
  , height : Int
  , width : Int
  }

init : { rows : Int, cols : Int } -> Cursor
init dims =
    { x = 0
    , y = 0
    , dir = Across
    , height = dims.cols
    , width = dims.rows
    }

toggle_dir : Cursor -> Cursor
toggle_dir cursor =
  let dir' = if cursor.dir == Across then Down else Across
  in
      { cursor | dir = dir' }

move : Int -> Int -> Cursor -> Cursor
move dx dy cursor =
  let x' = (cursor.x + dx) % cursor.width
      y' = (cursor.y + dy) % cursor.height
  in
      { cursor | x = x', y = y' }

advance : Cursor -> Cursor
advance cursor =
  case cursor.dir of
    Across -> move 1 0 cursor
    Down -> move 0 1 cursor
  
retreat : Cursor -> Cursor
retreat cursor =
  case cursor.dir of
    Across -> move 1 0 cursor
    Down -> move 0 1 cursor

set : Int -> Int -> Cursor -> Cursor
set x' y' cursor =
  { cursor | x = x', y = y' }
