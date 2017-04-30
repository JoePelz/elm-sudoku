import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type SudoNum = Blank | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Bad
type alias Box = { contents: SudoNum, valid: Bool, locked: Bool }
type alias BoxRow = List Box
type alias Square = List BoxRow
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
        [br] -> if y == 0 then [(changeBoxRow br x val)] else [br]
        (br::brs) -> if y == 0 then (changeBoxRow br x val) :: brs else br :: (changeSquare brs x (y-1) val)

changeBoxRow : BoxRow -> Int -> SudoNum -> BoxRow
changeBoxRow br x val =
    case br of
        [] -> []
        [b] -> if x == 0 then [changeCell b val] else [b]
        (b::bs) -> if x == 0 then (changeCell b val) :: bs else b :: (changeBoxRow bs (x-1) val)

changeCell : Box -> SudoNum -> Box
changeCell b val =
    case val of
        Bad -> b
        _ -> { b | contents = val }

-- VIEW

printBox : Int -> Int -> Box -> Html Msg
printBox y x box = input [onInput (Change x y), value (sudoNumToString box.contents)] []

printRow : Int -> BoxRow -> List (Html Msg)
printRow y row = List.indexedMap (printBox y) row

printSquare : Square -> Html Msg
printSquare square = div [] (List.indexedMap (\y srow -> div [] (printRow y srow)) square)

view : Model -> Html Msg
view model = printSquare model
-- view model = div [] (printRow model)
-- view model = div [] [ printBox 0 model ]

