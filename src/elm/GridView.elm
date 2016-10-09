module GridView exposing (svg_grid)

import Model exposing (..)
import Xword exposing (..)

import Svg.Attributes exposing (
  version, height, width, transform, style, class,
  x, y, r, cx, cy
  )
import Svg exposing (Svg, circle, rect, svg, g, line, text, text')
import Html.Events exposing (onClick)
import Html exposing (Html)

-- GRID --

top_left = { x = 0, y = 0 }

square_size = 32

grid_style = "crosspadGrid"
white_style = "crosspadWhite"
black_style = "crosspadBlack"
cwhite_style = "crosspadCursorWhite"
cblack_style = "crosspadCursorBlack"
square_style = "crosspadSquare"

cellstyle : Int -> Int -> Model -> String
cellstyle x y model =
  let cell = get_cell x y model.xw.grid
      is_cur = model.cur_x == x && model.cur_y == y
  in
      case (cell, is_cur) of
        (Black, False) -> black_style
        (Black, True) -> cblack_style
        (_, False) -> white_style
        (_, True) -> cwhite_style

cell model x' y' =
  let s = square_size
      x0 = top_left.x
      y0 = top_left.y
      x0' = x0 + x' * s
      y0' = y0 + y' * s
      cs = s |> toString
      cx = x0' |> toString
      cy = y0' |> toString
      num_x = x0' + 1 |> toString
      num_y = y0' + round (s/3) |> toString
      let_x = x0' + round (s/2) |> toString
      let_y = y0' + s - 5 |> toString
      xw = model.xw
      grid = xw.grid
      cstyle = square_style ++ " " ++ cellstyle x' y' model
      letter = get_letter x' y' grid
      number = get_number x' y' grid
  in
  g []
    [
      text' [ class "crosspadNumber", x num_x, y num_y ] [text number]
    , text' [ class "crosspadLetter", x let_x, y let_y ] [text letter]
    , rect [ class cstyle, x cx, y cy, width cs, height cs
           , onClick (ClickSquare x' y')
           ] []
    ]

cells model = 
  List.concatMap (\y ->
    List.map (\x -> cell model x y)
      [0..14])
    [0..14]

svg_grid : Model -> Html Msg
svg_grid model =
  let s = square_size
      w = model.width * s
      h = model.height * s
      w' = toString w
      h' = toString h
      x0' = toString top_left.x
      y0' = toString top_left.y
      svg_h = toString (h + 20) ++ "px"
      svg_w = toString (w + 20) ++ "px"
      box = rect [class grid_style, x x0', y y0', width w', height h'] []
  in
      svg
        [ version "1.1", x "0", y "0", height svg_h, width svg_w ]
        [ g [transform "translate(0.5, 0.5)"]
            ( [box] ++ cells model )
        ]
