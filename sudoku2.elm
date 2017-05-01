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
type alias Model = Square

model : Model
model =
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
        Change x y s -> changeSquare model x y (stringToSudoNum s)

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

--rowToNumbes : CellRow

--validateSquare : Square -> Square
--validateSquare sq =
--    let
--        numbers = List.map (\val -> row

-- need to find any numbers that appear more than once




-- VIEW

printCell : Int -> Int -> Cell -> Html Msg
printCell y x cell = input [onInput (Change x y), value (sudoNumToString cell.contents)] []

printRow : Int -> CellRow -> List (Html Msg)
printRow y row = List.indexedMap (printCell y) row

printSquare : Square -> Html Msg
printSquare square = div [class "square"] (List.indexedMap (\y srow -> div [] (printRow y srow)) square)

view : Model -> Html Msg
view model =
    let
        hero = printSquare model
    in
        div [] [stylesheet, hero]


-- view model = div [] (printRow model)
-- view model = div [] [ printCell 0 model ]

