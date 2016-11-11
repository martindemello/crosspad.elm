module Reader exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)

import Xword exposing (..)

type alias InCell =
  { x : Int
  , y : Int
  , contents : String
  }

type alias InXword =
  { rows : Int
  , cols : Int
  , cells : List InCell
  , across : List String
  , down : List String
  }

cell_decoder : Decoder InCell
cell_decoder =
  succeed InCell
    |: ("x" := int)
    |: ("y" := int)
    |: ("contents" := string)

xword_decoder : Decoder InXword
xword_decoder =
  succeed InXword
    |: ("cols" := int)
    |: ("rows" := int)
    |: ("cells" := list cell_decoder)
    |: ("across" := list string)
    |: ("down" := list string)

to_square : String -> Square
to_square s =
  case s of
    "#" -> black_square
    "." -> empty_square
    _ -> letter_square s

to_xword : InXword -> Xword
to_xword input =
  let xword = make_xword input
      grid' = List.foldl (\c g ->
          set_square c.x c.y (to_square c.contents) g)
        xword.grid
        input.cells
  in
      { xword | grid = grid', across = input.across, down = input.down }

decode : Value -> Result String Xword
decode json =
  decodeValue xword_decoder json
    |> Result.map to_xword
