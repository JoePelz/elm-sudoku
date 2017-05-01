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
type alias Model = Square

sudoNumCompare a b =
    let
        sa = sudoNumToString a
        sb = sudoNumToString b
    in
        compare sa sb

model : Model
model = validateSquare
    [   [ { contents = Blank, valid = True, locked = False }
        , { contents = Blank, valid = True, locked = False }
        , { contents = Blank, valid = True, locked = False }
        ]
    ,   [ { contents = Blank, valid = True, locked = False }
        , { contents = Blank, valid = True, locked = False }
        , { contents = Blank, valid = True, locked = False }
        ]
    ,   [ { contents = Blank, valid = True, locked = False }
        , { contents = Blank, valid = True, locked = False }
        , { contents = Blank, valid = True, locked = False }
        ]
    ]

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

type Msg = Change Int Int String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Change x y s -> updateSquare model x y (stringToSudoNum s)

updateSquare : Square -> Int -> Int -> SudoNum -> Square
updateSquare sq x y val =
    let
        updated = changeSquare sq x y val
    in
        validateSquare updated

changeSquare : Square -> Int -> Int -> SudoNum -> Square
changeSquare sq x y val =
    case sq of
        [] -> []
        [br] -> if y == 0 then [(changeCellRow br x val)] else [br]
        (br::brs) -> if y == 0 then (changeCellRow br x val) :: brs else br :: (changeSquare brs x (y-1) val)

changeCellRow : CellRow -> Int -> SudoNum -> CellRow
changeCellRow br x val =
    case br of
        [] -> []
        [b] -> if x == 0 then [changeCell b val] else [b]
        (b::bs) -> if x == 0 then (changeCell b val) :: bs else b :: (changeCellRow bs (x-1) val)

changeCell : Cell -> SudoNum -> Cell
changeCell b val =
    case val of
        Bad -> b
        _ -> { b | contents = val }

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

-- q = [One, Two, Three, Four, Four, Two, Four, Six, Blank, Blank, Bad]

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

-- need to find any numbers that appear more than once




-- VIEW

printCell : Int -> Int -> Int -> Int -> Cell -> Html Msg
printCell sx sy cy cx cell =
    if List.member cell.contents [Blank, Bad] then
        input
            [ onInput (Change (sx * 3 + cx) (sy * 3 + cy))
            , value (sudoNumToString cell.contents)
            , classList
                [ ("cell", True)
                , ("cell-empty", List.member cell.contents [Blank, Bad])
                ]
            ] []
    else if cell.locked then
        input
            [ onInput (Change (sx * 3 + cx) (sy * 3 + cy))
            , value (sudoNumToString cell.contents)
            , classList
                [ ("cell", True)
                , ("cell-locked", cell.locked)
                ]
            ] []
    else
        input
            [ onInput (Change (sx * 3 + cx) (sy * 3 + cy))
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
        hero = printSquare 0 0 model
    in
        div [] [stylesheet, hero]


-- view model = div [] (printRow model)
-- view model = div [] [ printCell 0 model ]

