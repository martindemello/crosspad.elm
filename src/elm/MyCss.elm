module MyCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li)
import Css.Namespace exposing (namespace)

type CssClasses =
  Square | Black | White | CursorBlack | CursorWhite
    | Number | Letter

str = toString

rgb' r g b = "rgb(" ++ (str r) ++ "," ++ (str g) ++ "," ++ (str b) ++ ")"

gridtext s =
  [ fontFamilies [ "arial" ]
  , fontSize s
  ]

fill c = property "fill" (rgb' c.red c.green c.blue)

fillOp c =
  [ fill c
  , property "fill-opacity" (str c.alpha)
  ]

-- we don't want the mathematical rem here
rem = Css.rem

css =
    (stylesheet << namespace "crosspad")
    [ body
        [ overflowX auto
        , minWidth (px 800)
        ]
    , (.) Square
        [ margin zero
        , padding zero
        , property "stroke-width" "1"
        , property "stroke" (rgb' 0 0 0)
        ]
    , (.) Black [ fill (rgb 0 0 0) ]
    , (.) White [ fill (rgb 255 255 255) ]
    , (.) CursorBlack (fillOp (rgba 0 0 128 0.5))
    , (.) CursorWhite (fillOp (rgba 128 128 255 0.5))
    , (.) Number (gridtext (rem 0.625))
    , (.) Letter (gridtext (rem 1.0))
    ]
