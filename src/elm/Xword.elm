module Xword exposing (..)

import Array2D
import Char exposing (..)
import String exposing (..)

-- TYPES --

type Cell = Black | Empty | Letter String

type alias Bars =
  { right : Bool
  , down : Bool
  }

empty_bars = { right = False, down = False }

type alias Square =
  { cell : Cell
  , bars : Bars
  , num : Int
  }

type alias Grid = Array2D.Array2D Square

type alias Xword =
  { rows : Int
  , cols : Int
  , grid : Grid
  , across : List String
  , down : List String
  , nums_ac : List Int
  , nums_dn : List Int
  }

empty_square = { cell = Empty, bars = empty_bars, num = 0 }
black_square = { empty_square | cell = Black }
letter_square s = { empty_square | cell = Letter s }

make_xword : { a | rows : Int, cols : Int } -> Xword
make_xword sz =
  let grid = Array2D.repeat sz.cols sz.rows empty_square
  in
      { rows = sz.rows
      , cols = sz.cols
      , grid = grid
      , across = []
      , down = []
      , nums_ac = []
      , nums_dn = []
      }
      |> renumber

-- grid accessors --

get_square x y grid =
  case Array2D.get x y grid of
    Nothing -> empty_square
    Just s -> s

get_cell x y grid =
  (get_square x y grid).cell

get_num x y grid =
  (get_square x y grid).num

is_black x y grid =
  case get_cell x y grid of
    Black -> True
    _ -> False

get_letter x y grid = 
  case get_cell x y grid of
    Letter s -> s
    _ -> ""

get_number x y grid = 
  case get_num x y grid of
    0 -> ""
    n -> toString n

set_square x y sq grid =
  Array2D.set x y sq grid

set_letter x y c grid =
  let s = String.fromChar c |> String.toUpper
      sq = get_square x y grid
      sq' = { sq | cell = Letter s }
  in
      set_square x y sq' grid

set_num x y n grid =
  let sq = get_square x y grid
      sq' = { sq | num = n }
  in
      set_square x y sq' grid

-- symmetry --
symm_180 x y xw =
  let xmax = xw.cols - 1
      ymax = xw.rows - 1
  in
  [ (x, y)
  , (xmax - x, ymax - y)
  ]

symm_90 x y xw =
  let xmax = xw.cols - 1
      ymax = xw.rows - 1
  in
  [ (x, y)
  , (xmax - x, ymax - y)
  , (xmax - y, x)
  , (y, ymax - x)
  ]

-- numbering --

is_boundary x y xw =
  (x < 0) || (y < 0) ||
  (x >= xw.cols) || (y >= xw.rows) ||
  is_black x y xw.grid

non_boundary x y xw =
  not <| is_boundary x y xw

starts_across x y xw =
  is_boundary (x - 1) y xw &&
  non_boundary x y xw &&
  non_boundary (x + 1) y xw

starts_down x y xw =
  is_boundary x (y - 1) xw &&
  non_boundary x y xw &&
  non_boundary x (y + 1) xw

-- renumber a single square --
--   helper function for renumber
--   returns (n+1, xw) if it sets num, (n, xw) otherwise
renumber_square x y n xw =
  let ac = starts_across x y xw
      dn = starts_down x y xw
      num = ac || dn
      n' = if num then n + 1 else n
      nac = if ac then n' :: xw.nums_ac else xw.nums_ac
      ndn = if dn then n' :: xw.nums_dn else xw.nums_dn
      xw' = if num then
        { xw | grid = set_num x y n' xw.grid
        , nums_ac = nac, nums_dn = ndn
        }
      else
        xw
  in
      (n', xw')

renumber xw =
  let (n', xw') =
        List.foldl (\y (n', xw') ->
          List.foldl (\x (n'', xw'') ->
            renumber_square x y n'' xw'')
          (n', xw')
          [0..(xw.rows - 1)])
        (0, { xw | nums_ac = [], nums_dn = [] })
        [0..(xw.cols - 1)]
  in
      { xw' | nums_ac = List.reverse xw'.nums_ac
      , nums_dn = List.reverse xw'.nums_dn
      }

-- zip the list of clues with the clue numbers, padding
-- with blank lines if necessary.
clue_list : Xword -> { across : List String, down : List String }
clue_list xw =
  let zip xs ys =
        let len = List.length in
        if len ys < len xs then
          zip xs <| ys ++ (List.repeat (len xs - len ys) "")
        else
          List.map2 (\x y -> (toString x) ++ ". " ++ y) xs ys
  in
      { across = zip xw.nums_ac xw.across
      , down = zip xw.nums_dn xw.down
      }
