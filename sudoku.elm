import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Debug exposing (..)


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
type alias Cell = { contents: SudoNum, valid: Bool, validRow: Bool, validCol: Bool, locked: Bool }
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

init_cell : SudoNum -> Cell
init_cell val = { contents = val, valid = True, validRow = True, validCol = True, locked = False }
init_cell_row = [init_cell Blank, init_cell Blank, init_cell Blank]
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

sudokuToSquares : Sudoku -> List Square
sudokuToSquares sud = List.concat sud

xformSquares : List (List a) -> List (List a)
xformSquares part =
    case part of
        [] -> []
        xs -> List.append (transpose (List.take 3 part)) (xformSquares (List.drop 3 part))

-- sudokuToCells : List (List (List number)) -> List number
sudokuToCells : Sudoku -> List Cell
sudokuToCells sud =
    let
        squares = sudokuToSquares sud
        partial = xformSquares squares
    in
        partial |> List.concat |> List.concat

groupN : Int -> List a -> List (List a)
groupN n cells =
    case cells of
        [] -> []
        xs -> (List.take n cells) :: groupN n (List.drop n cells)

cellsToSudoku : List Cell -> Sudoku
cellsToSudoku cells =
    let
        rows = groupN 3 cells
        groups = groupN 3 rows
    in
    case groups of
        [] -> []
        xs -> (transpose (List.take 3 groups)) :: cellsToSudoku (List.drop 27 cells)

transposeCells : List Cell -> List Cell
transposeCells cells =
    let
        rows = groupN 9 cells
    in
        transpose rows |> List.concat

-- UPDATE

type Msg = Change Int Int Int Int String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Change sx sy cx cy s -> updateSudoku model sx sy cx cy (stringToSudoNum s)

updateSudoku : Sudoku -> Int -> Int -> Int -> Int -> SudoNum -> Sudoku
updateSudoku sud sx sy cx cy val =
    let
        changed = changeSudoku sud sx sy cx cy val
        -- the above line also validates squares
    in
        validateLines changed

validateSquare : Square -> Square
validateSquare square =
    let
        numbers = List.concatMap (List.map .contents) square
        duplicates = findDuplicates numbers
    in
        List.map (setValidCellRow duplicates) square

validateLines : Sudoku -> Sudoku
validateLines sud =
    let
        cells = sudokuToCells sud
        rows = cellListToRows cells
        cols = transpose rows
        badrows = List.map findDuplicates rows
        badcols = List.map findDuplicates cols
    in
        cellsToSudoku <| markBadRows badrows <| markBadCols badcols cells

-- UPDATE helpers

-- for validateSquare
setValidCellRow : List SudoNum -> List Cell -> List Cell
setValidCellRow dup cr = List.map (setValidCell dup) cr

setValidCell : List SudoNum -> Cell -> Cell
setValidCell dup c = {c | valid = (not <| List.member c.contents dup)}

-- for validateRow
setValidRow : List SudoNum -> List Cell -> List Cell
setValidRow dup cr =
    let
        row_valid = List.length dup == 0
    in
        List.map (setValidRowCell row_valid dup) cr

setValidRowCell : Bool -> List SudoNum -> Cell -> Cell
setValidRowCell valid_row dup c =
    if List.member c.contents dup
    then
        {c | valid = False, validRow = valid_row}
    else
        {c | validRow = valid_row}

-- for validateCol
setValidCol : List SudoNum -> List Cell -> List Cell
setValidCol dup cr =
    let
        col_valid = List.length dup == 0
    in
        List.map (setValidColCell col_valid dup) cr

setValidColCell : Bool -> List SudoNum -> Cell -> Cell
setValidColCell valid_col dup c =
    if List.member c.contents dup
    then
        {c | valid = False, validCol = valid_col}
    else
        {c | validCol = valid_col}


cellListToRows : List Cell -> List (List SudoNum)
cellListToRows nums =
    case nums of
        [] -> []
        xs -> (List.map .contents <| List.take 9 nums) :: cellListToRows (List.drop 9 nums)

-- badness is a 9 lists, one per row of cells.
-- cells is a single list of 81 cells.

markBadRows : List (List SudoNum) -> List Cell -> List Cell
markBadRows badness cells =
    let
        heads = List.take 9 cells
        tails = List.drop 9 cells
    in
    case badness of
        [] -> []
        [x] -> setValidRow x heads
        (x::xs) -> List.append (setValidRow x heads) (markBadRows xs tails)

-- need to iterate over

markBadCols_helper badness cells =
    let
        heads = List.take 9 cells
        tails = List.drop 9 cells
    in
    case badness of
        [] -> []
        [x] -> setValidCol x heads
        (x::xs) -> List.append (setValidCol x heads) (markBadCols_helper xs tails)

markBadCols : List (List SudoNum) -> List Cell -> List Cell
markBadCols badness cells = transposeCells (markBadCols_helper badness (transposeCells cells))

-- START: Change cell value  code
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
        [s] -> if sx == 0 then [validateSquare <| changeSquare s cx cy val] else [s]
        (s::ss) -> if sx == 0 then (validateSquare <| changeSquare s cx cy val) ::  ss else s :: (changeSquareRow ss (sx-1) cx cy val)

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
-- END: Change cell value code

-- OTHER HELPERS

-- preconditions: list is sorted; "==" will detect equality
repeats : List SudoNum -> List SudoNum
repeats l =
    case l of
        [] -> []
        [x] -> []
        [x, y] -> if x==y && x /= Blank then [x] else []
        (x::y::xs) -> if x==y && x /= Blank then x :: repeats xs else repeats (y::xs)

-- preconditions: list is sorted; "==" will detect equality.
uniquify : List a -> List a
uniquify l =
    case l of
        [] -> []
        [x] -> [x]
        [x, y] -> if x==y then [x] else [x, y]
        (x::y::xs) -> if x == y then x :: uniquify xs else x :: uniquify (y::xs)

transpose : List (List a) -> List (List a)
transpose ll =
  case ll of
    [] -> []
    ([]::xss) -> transpose xss
    ((x::xs)::xss) ->
      let
        heads = List.filterMap List.head xss
        tails = List.filterMap List.tail xss
      in
        (x::heads)::transpose (xs::tails)

findDuplicates : List SudoNum -> List SudoNum
findDuplicates numbers =
    let
        sorted = List.sortWith sudoNumCompare numbers
    in
        uniquify (repeats sorted)

-- VIEW

printCell : Int -> Int -> Int -> Int -> Cell -> Html Msg
printCell sx sy cy cx cell =
    if cell.locked then
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
                , ("cell-empty", cell.contents == Blank || cell.contents == Bad)
                , ("cell-valid", cell.valid)
                , ("cell-invalid", not cell.valid)
                , ("cell-invalid-row", not cell.validRow)
                , ("cell-invalid-col", not cell.validCol)
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

