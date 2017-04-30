import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type SudoNum = Blank | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Bad
type alias Box = { contents: SudoNum, valid: Bool, locked: Bool }
type alias BoxRow = List Box
type alias Model = BoxRow

model : Model
model =
    [ { contents = Blank, valid = True, locked = False }
    , { contents = Blank, valid = True, locked = False }
    , { contents = Blank, valid = True, locked = False }
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

type Msg = Change Int String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Change i s -> change model i (stringToSudoNum s)

change : BoxRow -> Int -> SudoNum -> BoxRow
change b i val =
    case b of
        [] -> []
        [x] -> if i == 0 then [{ x | contents = val }] else [x]
        (x::xs) -> if i == 0 then { x | contents = val } :: xs else x :: (change xs (i-1) val)
--    case val of
--        Bad -> b
--        _ -> { b | contents = val }

-- VIEW

printBox : Int -> Box -> Html Msg
printBox n box = input [onInput (Change n), value (sudoNumToString box.contents)] []

printRow : BoxRow -> List (Html Msg)
printRow row = List.indexedMap printBox row

view : Model -> Html Msg
view model = div [] (printRow model)
-- view model = div [] [ printBox 0 model ]

