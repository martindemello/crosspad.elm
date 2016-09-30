module Keycode exposing (..)

type Key =
  Ascii Char
  | Alt | Ctrl | Shift | Meta | Escape | CapsLock
  | Space | Enter | Tab | Backspace | Delete | Insert
  | ArrowLeft | ArrowRight | ArrowUp | ArrowDown | Home | End | PageUp | PageDown
  | Unknown

fromKeyCode : Int -> Key
fromKeyCode c =
  case c of
    08 -> Backspace
    09 -> Tab
    13 -> Enter
    16 -> Shift
    17 -> Ctrl
    18 -> Alt
    20 -> CapsLock
    27 -> Escape
    32 -> Space
    33 -> PageUp
    34 -> PageDown
    35 -> End
    36 -> Home
    37 -> ArrowLeft
    38 -> ArrowUp
    39 -> ArrowRight
    40 -> ArrowDown
    45 -> Insert
    46 -> Delete
    91 -> Meta
    _  -> Unknown

