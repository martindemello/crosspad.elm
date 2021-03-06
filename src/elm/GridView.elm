module GridView exposing (svg_grid)

import Set
import Model exposing (..)
import Types exposing (..)
import Utils exposing (..)
import Xword exposing (..)
import Svg.Attributes
    exposing
        ( version
        , height
        , width
        , transform
        , style
        , class
        , x
        , y
        , r
        , cx
        , cy
        )
import Svg exposing (Svg, circle, rect, svg, g, line, text, text_)
import Html.Events exposing (onClick)
import Html exposing (Html)


-- GRID --


top_left =
    { x = 0, y = 0 }


square_size =
    32


cellstyle : Int -> Int -> Model -> String
cellstyle x y model =
    let
        cell =
            get_cell x y model.xw.grid

        is_cur =
            (model.cursor.x == x && model.cursor.y == y)

        is_word =
            (not is_cur) && (Set.member ( x, y ) model.current_word)

        bg =
            when (cell == Black) "black" "white"

        cursor =
            when is_cur "cursor-" ""
    in
        if is_word then
            "crosspad-word"
        else
            "crosspad-" ++ cursor ++ bg


cell : Model -> Int -> Int -> Svg Msg
cell model x_ y_ =
    let
        s =
            square_size

        x0 =
            top_left.x

        y0 =
            top_left.y

        x0_ =
            x0 + x_ * s

        y0_ =
            y0 + y_ * s

        cs =
            s |> toString

        cx =
            x0_ |> toString

        cy =
            y0_ |> toString

        num_x =
            x0_ + 1 |> toString

        num_y =
            y0_ + round (s / 3) |> toString

        let_x =
            x0_ + round (s / 2) |> toString

        let_y =
            y0_ + s - 5 |> toString

        xw =
            model.xw

        grid =
            xw.grid

        cstyle =
            "crosspad-square " ++ cellstyle x_ y_ model

        letter =
            get_letter x_ y_ grid

        number =
            get_number x_ y_ grid
    in
        g []
            [ text_ [ class "crosspad-number", x num_x, y num_y ] [ text number ]
            , text_ [ class "crosspad-letter", x let_x, y let_y ] [ text letter ]
            , rect
                [ class cstyle
                , x cx
                , y cy
                , width cs
                , height cs
                , onClick (ClickSquare x_ y_)
                ]
                []
            ]


cells : Model -> List (Svg Msg)
cells model =
    List.concatMap
        (\y ->
            List.map (\x -> cell model x y)
                (List.range 0 (model.xw.cols - 1))
        )
        (List.range 0 (model.xw.rows - 1))


svg_grid : Model -> Html Msg
svg_grid model =
    let
        s =
            square_size

        w =
            model.xw.cols * s

        h =
            model.xw.rows * s

        w_ =
            toString w

        h_ =
            toString h

        x0_ =
            toString top_left.x

        y0_ =
            toString top_left.y

        svg_h =
            toString (h + 1) ++ "px"

        svg_w =
            toString (w + 1) ++ "px"

        box =
            rect [ class "crosspad-grid", x x0_, y y0_, width w_, height h_ ] []
    in
        svg
            [ version "1.1", x "0", y "0", height svg_h, width svg_w ]
            [ g [ transform "translate(0.5, 0.5)" ]
                ([ box ] ++ cells model)
            ]
