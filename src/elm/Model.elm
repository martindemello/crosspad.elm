module Model exposing (..)

import Array2D
import Char exposing (..)
import String exposing (..)
import Cursor exposing (Cursor)
import Keycode exposing (..)
import Types exposing (..)
import Xword exposing (..)


-- MODEL --


type alias Model =
    { width : Int
    , height : Int
    , xw : Xword
    , cursor : Cursor
    , symmetry : Symmetry
    , load_format : String
    , save_format : String
    , grid_active : Bool
    }


init : { rows : Int, cols : Int } -> Model
init dims =
    let
        xw =
            make_xword dims

        cursor =
            Cursor.init dims

        hd xs =
            List.head xs |> Maybe.withDefault ""
    in
        { width = dims.cols
        , height = dims.rows
        , xw = xw
        , cursor = cursor
        , symmetry = Symm180
        , load_format = hd load_formats
        , save_format = hd save_formats
        , grid_active = True
        }


load_formats : List String
load_formats =
    [ "across-lite-binary", "across-lite-text", "reddit-blank", "qxw" ]


save_formats : List String
save_formats =
    [ "across-lite-binary", "across-lite-text", "reddit-blank", "reddit-filled" ]



-- UPDATE --


is_current_black model =
    is_black model.cursor.x model.cursor.y model.xw.grid


set_current_square : Square -> Model -> Model
set_current_square sq model =
    let
        x =
            model.cursor.x

        y =
            model.cursor.y

        xw =
            model.xw

        g =
            set_square x y sq xw.grid

        xw_ =
            { xw | grid = g }
    in
        { model | xw = xw_ }


update_letter : Char -> Model -> Model
update_letter c model =
    let
        s =
            String.fromChar c |> String.toUpper

        sq =
            letter_square s
    in
        if is_current_black model then
            model
        else
            set_current_square sq model


update_cursor f model =
    { model | cursor = f model.cursor }


move_cursor dx dy model =
    update_cursor (Cursor.move dx dy) model


advance_cursor model =
    update_cursor Cursor.advance model


retreat_cursor model =
    update_cursor Cursor.retreat model


set_cursor x y model =
    update_cursor (Cursor.set x y) model


toggle_black : Model -> Model
toggle_black model =
    let
        x =
            model.cursor.x

        y =
            model.cursor.y

        xw =
            model.xw

        sq =
            if is_black x y xw.grid then
                empty_square
            else
                black_square

        squares =
            case model.symmetry of
                GridLocked ->
                    []

                SymmNone ->
                    [ ( x, y ) ]

                Symm90 ->
                    symm_90 x y xw

                Symm180 ->
                    symm_180 x y xw

        grid_ =
            List.foldl
                (\( i, j ) grid ->
                    set_square i j sq grid
                )
                xw.grid
                squares

        xw_ =
            { xw | grid = grid_ }
    in
        { model | xw = xw_ } |> advance_cursor


update_numbers : Model -> Model
update_numbers model =
    let
        xw_ =
            model.xw |> renumber
    in
        { model | xw = xw_ }


toggle_dir : Model -> Model
toggle_dir model =
    update_cursor Cursor.toggle_dir model


delete_current : Model -> Model
delete_current model =
    if is_current_black model then
        model
    else
        set_current_square empty_square model |> update_numbers


backspace_current : Model -> Model
backspace_current model =
    delete_current model |> retreat_cursor


handle_keycode k model =
    if model.grid_active then
        case k of
            ArrowLeft ->
                move_cursor -1 0 model

            ArrowRight ->
                move_cursor 1 0 model

            ArrowUp ->
                move_cursor 0 -1 model

            ArrowDown ->
                move_cursor 0 1 model

            Space ->
                toggle_black model |> update_numbers

            PageDown ->
                toggle_dir model

            Delete ->
                delete_current model

            Backspace ->
                backspace_current model

            _ ->
                model
    else
        model


handle_keypress c model =
    if model.grid_active then
        if Char.isUpper (c) || Char.isLower (c) || Char.isDigit (c) then
            update_letter c model
                |> update_numbers
                |> advance_cursor
        else
            model
    else
        model


set_grid_focus focused model =
    { model | grid_active = focused }


handle_symm symm model =
    { model | symmetry = symm }


handle_load_format_changed fmt model =
    { model | load_format = fmt }


handle_save_format_changed fmt model =
    { model | save_format = fmt }
