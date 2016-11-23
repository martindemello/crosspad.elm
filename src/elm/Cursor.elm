module Cursor exposing (..)

import Types exposing (..)
import Utils exposing (..)


type alias Cursor =
    { x : Int
    , y : Int
    , dir : Direction
    , height : Int
    , width : Int
    }


init : { a | rows : Int, cols : Int } -> Cursor
init dims =
    { x = 0
    , y = 0
    , dir = Across
    , height = dims.cols
    , width = dims.rows
    }


set_dir : Direction -> Cursor -> Cursor
set_dir direction cursor =
    { cursor | dir = direction }


toggle_dir : Cursor -> Cursor
toggle_dir cursor =
    let
        dir_ =
            when (cursor.dir == Across) Down Across
    in
        { cursor | dir = dir_ }


type alias Move =
    Int -> Int -> Cursor -> Cursor


move : Move
move dx dy cursor =
    let
        x_ =
            (cursor.x + dx) % cursor.width

        y_ =
            (cursor.y + dy) % cursor.height
    in
        { cursor | x = x_, y = y_ }


move_nowrap : Move
move_nowrap dx dy cursor =
    let
        x_ =
            cursor.x + dx

        y_ =
            cursor.y + dy
    in
        { cursor | x = x_, y = y_ }


advance_ : Move -> Cursor -> Cursor
advance_ move_fn cursor =
    case cursor.dir of
        Across ->
            move_fn 1 0 cursor

        Down ->
            move_fn 0 1 cursor


advance : Cursor -> Cursor
advance cursor =
    advance_ move cursor


advance_nowrap : Cursor -> Cursor
advance_nowrap cursor =
    advance_ move_nowrap cursor


retreat_ : Move -> Cursor -> Cursor
retreat_ move_fn cursor =
    case cursor.dir of
        Across ->
            move_fn -1 0 cursor

        Down ->
            move_fn 0 -1 cursor


retreat : Cursor -> Cursor
retreat cursor =
    retreat_ move cursor


retreat_nowrap : Cursor -> Cursor
retreat_nowrap cursor =
    retreat_ move_nowrap cursor


set : Int -> Int -> Cursor -> Cursor
set x_ y_ cursor =
    { cursor | x = x_, y = y_ }
