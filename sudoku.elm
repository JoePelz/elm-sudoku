import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


-- temp CSS for dev


stylesheet =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "sudoku.css"
            ]
        children = []
    in
        node tag attrs children



main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type SudoNum = Blank | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Bad
type alias Cell = { contents: SudoNum, valid: Bool, locked: Bool }
type alias CellRow = List Cell
type alias Square = List CellRow
type alias SquareRow = List Square
type alias Sudoku = List SquareRow
type alias Model = Sudoku

sudoNumCompare a b =
    let
        sa = sudoNumToString a
        sb = sudoNumToString b
    in
        compare sa sb

init_cell = { contents = Blank, valid = True, locked = False }
init_cell_row = [init_cell, init_cell, init_cell]
init_square = [init_cell_row, init_cell_row, init_cell_row]
init_square_row = [validateSquare init_square, validateSquare init_square, validateSquare init_square]
init_sudoku = [init_square_row, init_square_row, init_square_row]

model : Model
model = init_sudoku

stringToSudoNum : String -> SudoNum
stringToSudoNum s =
    let r = String.right 1 s
    in
    case r of
        "1" -> One
        "2" -> Two
        "3" -> Three
        "4" -> Four
        "5" -> Five
        "6" -> Six
        "7" -> Seven
        "8" -> Eight
        "9" -> Nine
        "" -> Blank
        _ -> Bad

sudoNumToString : SudoNum -> String
sudoNumToString sn =
    case sn of
        One -> "1"
        Two -> "2"
        Three -> "3"
        Four -> "4"
        Five -> "5"
        Six -> "6"
        Seven -> "7"
        Eight -> "8"
        Nine -> "9"
        Blank -> ""
        Bad -> ""

-- UPDATE

type Msg = Change Int Int Int Int String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Change sx sy cx cy s -> updateSudoku model sx sy cx cy (stringToSudoNum s)

updateSudoku : Sudoku -> Int -> Int -> Int -> Int -> SudoNum -> Sudoku
updateSudoku sud sx sy cx cy val = changeSudoku sud sx sy cx cy val

updateSquare : Square -> Int -> Int -> SudoNum -> Square
updateSquare sq cx cy val =
    let
        updated = changeSquare sq cx cy val
    in
        validateSquare updated

changeSudoku : Sudoku -> Int -> Int -> Int -> Int -> SudoNum -> Sudoku
changeSudoku sud sx sy cx cy val =
    case sud of
        [] -> []
        [sr] -> if sy == 0 then [(changeSquareRow sr sx cx cy val)] else [sr]
        (sr::srs) -> if sy == 0 then (changeSquareRow sr sx cx cy val) :: srs else sr :: (changeSudoku srs sx (sy-1) cx cy val)

changeSquareRow : SquareRow -> Int -> Int -> Int -> SudoNum -> SquareRow
changeSquareRow sr sx cx cy val =
    case sr of
        [] -> []
        [s] -> if sx == 0 then [updateSquare s cx cy val] else [s]
        (s::ss) -> if sx == 0 then (updateSquare s cx cy val) ::  ss else s :: (changeSquareRow ss (sx-1) cx cy val)

changeSquare : Square -> Int -> Int -> SudoNum -> Square
changeSquare s cx cy val =
    case s of
        [] -> []
        [cr] -> if cy == 0 then [(changeCellRow cr cx val)] else [cr]
        (cr::crs) -> if cy == 0 then (changeCellRow cr cx val) :: crs else cr :: (changeSquare crs cx (cy-1) val)

changeCellRow : CellRow -> Int -> SudoNum -> CellRow
changeCellRow cr cx val =
    case cr of
        [] -> []
        [c] -> if cx == 0 then [changeCell c val] else [c]
        (c::cs) -> if cx == 0 then (changeCell c val) :: cs else c :: (changeCellRow cs (cx-1) val)

changeCell : Cell -> SudoNum -> Cell
changeCell c val =
    case val of
        Bad -> c
        _ -> { c | contents = val }

cellRowToNumbers : CellRow -> List SudoNum
cellRowToNumbers row = List.map .contents row

squareToNumbers : Square -> List SudoNum
squareToNumbers square = List.foldr List.append [] (List.map cellRowToNumbers square)

repeats : List SudoNum -> List SudoNum
repeats l =
    case l of
        [] -> []
        [x] -> []
        [x, y] -> if x==y then [x] else []
        (x::y::xs) -> if x==y then x :: repeats xs else repeats (y::xs)

uniquify : List SudoNum -> List SudoNum
uniquify l =
    case l of
        [] -> []
        [x] -> [x]
        [x, y] -> if x==y then [x] else [x, y]
        (x::y::xs) -> if x == y then x :: uniquify xs else x :: uniquify (y::xs)

findDuplicates : List SudoNum -> List SudoNum
findDuplicates numbers =
    let
        sorted = List.sortWith sudoNumCompare numbers
    in
        uniquify (repeats sorted)


setValidCell : List SudoNum -> Cell -> Cell
setValidCell dup c = {c | valid = (not <| List.member c.contents dup)}

setValidCellRow : List SudoNum -> CellRow -> CellRow
setValidCellRow dup cr = List.map (setValidCell dup) cr

validateSquare : Square -> Square
validateSquare square =
    let
        numbers = squareToNumbers square
        duplicates = findDuplicates numbers
    in
        List.map (setValidCellRow duplicates) square

-- VIEW

printCell : Int -> Int -> Int -> Int -> Cell -> Html Msg
printCell sx sy cy cx cell =
    if List.member cell.contents [Blank, Bad] then
        input
            [ onInput (Change sx sy cx cy)
            , value (sudoNumToString cell.contents)
            , classList
                [ ("cell", True)
                , ("cell-empty", List.member cell.contents [Blank, Bad])
                ]
            ] []
    else if cell.locked then
        input
            [ onInput (Change sx sy cx cy)
            , value (sudoNumToString cell.contents)
            , classList
                [ ("cell", True)
                , ("cell-locked", cell.locked)
                ]
            ] []
    else
        input
            [ onInput (Change sx sy cx cy)
            , value (sudoNumToString cell.contents)
            , classList
                [ ("cell", True)
                , ("cell-valid", cell.valid)
                , ("cell-invalid", not cell.valid)
                ]
            ] []

printCellRow : Int -> Int -> Int -> CellRow -> List (Html Msg)
printCellRow sx sy cy crow = List.indexedMap (printCell sx sy cy) crow

printSquare : Int -> Int -> Square -> Html Msg
printSquare sy sx square = div [class "square"] (List.indexedMap (\cy crow -> div [] (printCellRow sx sy cy crow)) square)

printSquareRow : Int -> SquareRow -> List (Html Msg)
printSquareRow sy srow = List.indexedMap (printSquare sy) srow

printSudoku : Sudoku -> Html Msg
printSudoku sudoku = div [class "sudoku"] (List.indexedMap (\sy srow -> div [] (printSquareRow sy srow)) sudoku)

view : Model -> Html Msg
view model =
    let
        hero = printSudoku model
    in
        div [] [stylesheet, hero]


-- view model = div [] (printRow model)
-- view model = div [] [ printCell 0 model ]

