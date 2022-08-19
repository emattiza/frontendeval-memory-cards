module Main exposing (main)

import Array exposing (Array, empty)
import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


type Flip
    = FaceUp
    | FaceDown


type Card
    = Card { flipState : Flip, value : Int }


newCard : Int -> Card
newCard val =
    Card { flipState = FaceDown, value = val }


mkPair : Card -> Card
mkPair (Card card) =
    Card card


flipCard : Card -> Card
flipCard (Card card) =
    case card.flipState of
        FaceUp ->
            Card { card | flipState = FaceDown }

        FaceDown ->
            Card { card | flipState = FaceUp }


viewCard : Card -> Html msg
viewCard (Card card) =
    case card.flipState of
        FaceUp ->
            div [] []

        FaceDown ->
            div [] [ text <| String.fromInt card.value ]


type Board
    = Grid { card : Array (Array Card), width : Int, height : Int }


viewBoard : Board -> Html msg
viewBoard (Grid board) =
    div [] [ text <| "Board: " ++ String.fromInt board.width ++ "x" ++ String.fromInt board.height ]


type Turn
    = Flip Card
    | NextFlip
    | Match
    | Fail
    | Complete


type alias Model =
    { board : Board, firstCard : Maybe Card, secondCard : Maybe Card }


initialModel : Model
initialModel =
    { board = Grid { card = Array.push empty empty, width = 6, height = 6 }, firstCard = Nothing, secondCard = Nothing }


type alias Msg =
    Turn


update : Msg -> Model -> Model
update msg model =
    case msg of
        _ ->
            model


view : Model -> Html Msg
view model =
    div [ class "btn-group" ]
        [ viewBoard model.board
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
