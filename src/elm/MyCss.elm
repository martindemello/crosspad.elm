module MyCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li)
import Css.Namespace exposing (namespace)

type CssClasses = GridContainer | Grid
  | Square | Black | White | CursorBlack | CursorWhite
  | Number | Letter
  | SettingsButtonActive | SettingsButtonInactive
  | StatusLabel | StatusBarContainer
  | ToolbarButton

str = toString

rgb' r g b = "rgb(" ++ (str r) ++ "," ++ (str g) ++ "," ++ (str b) ++ ")"

arial = fontFamilies [ "arial" ]

-- c.alpha doesn't get set properly by rgba
fill c alpha =
  [ property "fill" (rgb' c.red c.green c.blue)
  , property "fill-opacity" (str alpha)
  ]

button c = c ++
        [ padding (px 2)
        , margin (px 2)
        , border (px 2)
        , borderStyle solid
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
        , property "stroke" (rgb' 128 128 128)
        ]
    , (.) Grid
        [ property "fill" (rgb' 255 255 255)
        , property "stroke-width" "1"
        , property "stroke" (rgb' 128 128 128)
        ]
    , (.) GridContainer
        [ padding zero
        , margin (px 10)
        , borderColor (rgba 0 0 0 0.5)
        , border (px 2)
        ]
    , (.) Black (fill (rgb 0 0 0) 1)
    , (.) White (fill (rgb 255 255 255) 0)
    , (.) CursorBlack (fill (rgb 0 0 128) 0.5)
    , (.) CursorWhite (fill (rgb 128 128 255) 0.5)
    , (.) Number [ arial, letterSpacing zero, fontSize (rem 0.625) ]
    , (.) Letter
        [ arial
        , fontSize (rem 1.0)
        , property "text-anchor" "middle"
        ]
    , (.) SettingsButtonActive
        (button
          [ borderColor (rgb 61 146 201)
          , backgroundColor (rgba 61 146 201 0.8)
          ])
    , (.) SettingsButtonInactive
        (button
        [ borderColor (rgb 61 146 201)
        , backgroundColor (rgba 61 146 201 0.1)
        ])
    , (.) ToolbarButton
        (button
        [ borderColor (rgb 61 146 201)
        , backgroundColor (rgba 61 146 201 0.1)
        ])
    , (.) StatusLabel
        [ margin (px 2)
        , padding (px 2)
        , display inline
        , border (px 2)
        , borderStyle solid
        , borderColor (rgb 61 146 201)
        , backgroundColor (rgba 192 255 192 1)
        ]
    , (.) StatusBarContainer
        [ backgroundColor (rgb 240 240 240)
        , display inline
        ]

    ]
