module Writer exposing (..)

import Json.Encode exposing (..)
import Xword exposing (..)
import List exposing (range)


string_of_cell : Cell -> Value
string_of_cell c =
    case c of
        Black ->
            string "#"

        Empty ->
            string ""

        Letter s ->
            string s


cell : Int -> Int -> Cell -> Value
cell x y c =
    object
        [ ( "x", int x )
        , ( "y", int y )
        , ( "contents", string_of_cell c )
        ]


cells : Xword -> Value
cells xw =
    let
        g =
            List.foldl
                (\y acc ->
                    let
                        r =
                            List.foldl
                                (\x row ->
                                    (cell x y (get_cell x y xw.grid)) :: row
                                )
                                []
                                (range 0 (xw.rows - 1))
                    in
                        (List.reverse r) :: acc
                )
                []
                (range 0 (xw.cols - 1))
    in
        List.reverse g |> List.concat |> list


clues : List String -> Value
clues cs =
    List.map string cs |> list


to_json_string : Xword -> String
to_json_string xw =
    encode 0 <|
        object
            [ ( "rows", int xw.rows )
            , ( "cols", int xw.cols )
            , ( "cells", cells xw )
            , ( "across", clues xw.across )
            , ( "down", clues xw.down )
            ]
