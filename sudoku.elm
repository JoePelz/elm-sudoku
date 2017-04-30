import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type SudoNum = Blank | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
type alias Box = { contents: SudoNum, valid: Bool, locked: Bool }
type BoxRow = List Box
type alias Model = BoxRow

model : Model
model =
    [ { contents = Blank, valid = True, locked = False }
    , { contents = Blank, valid = True, locked = False }
    , { contents = Blank, valid = True, locked = False }
    ]

stringToSudoNum : String -> SudoNum
stringToSudoNum s =
    case s of
        "1" -> One
        "2" -> Two
        "3" -> Three
        "4" -> Four
        "5" -> Five
        "6" -> Six
        "7" -> Seven
        "8" -> Eight
        "9" -> Nine
        _ -> Blank

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

-- UPDATE

type Msg = Change Int String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Change n s -> change model n (stringToSudoNum msg)

change : BoxRow -> Int -> SudoNum -> BoxRow
change row i val =
    case row of
        [] -> []
        head :: rest ->
            if i == 0 then { head | contents = val } :: rest else head :: change rest (i-1) val


-- VIEW

printBox : Int -> Box -> Html msg
printBox n box = input [onInput (Change n)] [text (sudoNumToString box)]

printRow : BoxRow -> List (Html msg)
printRow row = List.indexedMap printBox row

view : Model -> Html Msg
view model = div [] (printRow model)

