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
    let
        dir_ =
            if cursor.dir == Across then
                Down
            else
                Across
    in
        { cursor | dir = dir_ }


move : Int -> Int -> Cursor -> Cursor
move dx dy cursor =
    let
        x_ =
            (cursor.x + dx) % cursor.width

        y_ =
            (cursor.y + dy) % cursor.height
    in
        { cursor | x = x_, y = y_ }


advance : Cursor -> Cursor
advance cursor =
    case cursor.dir of
        Across ->
            move 1 0 cursor

        Down ->
            move 0 1 cursor


retreat : Cursor -> Cursor
retreat cursor =
    case cursor.dir of
        Across ->
            move -1 0 cursor

        Down ->
            move 0 -1 cursor


set : Int -> Int -> Cursor -> Cursor
set x_ y_ cursor =
    { cursor | x = x_, y = y_ }
