module GridView exposing (svg_grid)

import Model exposing (..)
import Types exposing (..)
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


grid_style =
    "crosspadGrid"


white_style =
    "crosspadWhite"


black_style =
    "crosspadBlack"


cwhite_style =
    "crosspadCursorWhite"


cblack_style =
    "crosspadCursorBlack"


square_style =
    "crosspadSquare"


cellstyle : Int -> Int -> Model -> String
cellstyle x y model =
    let
        cell =
            get_cell x y model.xw.grid

        is_cur =
            model.cursor.x == x && model.cursor.y == y
    in
        case ( cell, is_cur ) of
            ( Black, False ) ->
                black_style

            ( Black, True ) ->
                cblack_style

            ( _, False ) ->
                white_style

            ( _, True ) ->
                cwhite_style


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
            square_style ++ " " ++ cellstyle x_ y_ model

        letter =
            get_letter x_ y_ grid

        number =
            get_number x_ y_ grid
    in
        g []
            [ text_ [ class "crosspadNumber", x num_x, y num_y ] [ text number ]
            , text_ [ class "crosspadLetter", x let_x, y let_y ] [ text letter ]
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
            rect [ class grid_style, x x0_, y y0_, width w_, height h_ ] []
    in
        svg
            [ version "1.1", x "0", y "0", height svg_h, width svg_w ]
            [ g [ transform "translate(0.5, 0.5)" ]
                ([ box ] ++ cells model)
            ]
