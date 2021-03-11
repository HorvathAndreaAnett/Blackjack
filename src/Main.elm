-----------------------
-- Horvath Andrea - Anett
-- 06.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Main exposing (main, calculateScore)

import Browser
import Html exposing (..)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (..)
import List exposing (..)
import Random
import Debug

import Card exposing (..)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { hand: List Card,
    deck: List Card,
    showDeck: Bool
  }

startingModel : Model
startingModel =
    Model [] Card.deck True

init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , Cmd.none
  )


type Msg
  = Draw
  | NewCard Card
  | ToggleDeck


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draw ->
      ( model
      , drawCard model
      )
    
    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewCard newCard ->
      ( --Debug.todo "Implement this"
      {model | hand = append model.hand [newCard], deck = filter (\x -> x /= newCard) model.deck, showDeck = model.showDeck}
      --Model (append model.hand [newCard]) (filter (\x -> x /= newCard) model.deck) (model.showDeck)
      , Cmd.none
      )

    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToggleDeck ->
      (
        --Debug.todo "Implement this"
        {model | hand = model.hand, deck = model.deck, showDeck = not model.showDeck}
        --Model (model.hand) (model.deck) (not model.showDeck)
      , Cmd.none
      )

drawCard : Model -> Cmd Msg
drawCard model =
  case model.deck of
    (first::rest) -> Random.generate NewCard (Random.uniform first rest)
    _ -> Cmd.none

{-
  1. Get the value of each card (use `cardValue`)
  2. Generate a list of all possible scores
  3. Return the score closest to 21 (less than 21!), if one exists, else the smallest score over 21
  ```elm
  calculateScore [Card King Hearts] == 10
  calculateScore [Card Two Hearts] == 2
  calculateScore [Card Two Hearts, Card King Spades] == 12
  calculateScore [Card Ace Hearts, Card King Spades] == 21
  calculateScore [Card Ace Hearts, Card Five Hears, Card Seven Spades] == 13
  calculateScore [Card King Hearts, Card Five Hears, Card Seven Spades] == 22
  calculateScore [Card King Hearts, Card Ten Clubs, Card Ace Spades] == 21
  calculateScore [Card Ace Spades, Card Ace Clubs, Card Ten Clubs, Card King Clubs] == 22
  ```
-}
calculateScore : List Card -> Int
calculateScore cards = --Debug.todo "Implement this"
    let
        calculateScoreHelper : List Card -> List Int -> List Int
        calculateScoreHelper list scoreList =
            case list of
                [] -> scoreList
                x::xs -> case x of
                            Card Ace _ -> calculateScoreHelper xs ((List.map (\y -> y + 1) scoreList) ++ (List.map (\z -> z + 11) scoreList))
                            _ -> case (head (cardValue x)) of
                                Nothing -> []
                                Just value -> calculateScoreHelper xs (List.map (\t -> t + value) scoreList)
    in
        case maximum (filter (\x -> x <= 21) (calculateScoreHelper cards [0])) of
            Nothing -> case minimum (filter (\x -> x > 21) (calculateScoreHelper cards [0])) of
                            Nothing -> 0
                            Just score -> score
            Just score -> score

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

{-
  Use the `viewCard` function for showing the player's hand and the `cardToUnicode` function for the remaining deck.
-}
view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"

    cards =
          model.hand
          |> List.map viewCard

    deck =
        model.deck
        |> List.map cardToUnicode
        |> String.concat

    score = calculateScore model.hand

  in
    div []
      [ div [] [ h1 [] [text appName] ]
      , button [ onClick Draw, disabled(score >= 21)] [ text "Draw card" ]
      , div [] [ if score > 21 then div [] [ h2[] [ text "Maybe next time..." ] ]
                 else if score == 21 then div [] [ h2[] [ text "Congratulations! You are a really lucky person :)" ] ]
                 else div [] [ h2[] [ text "" ] ] ]
      , div [] cards
      , div [] [ h2 [] [ text ("Score: " ++ String.fromInt score) ] ]
      , button [ onClick ToggleDeck ] [ text "Toogle deck" ]
      , div [] [ if model.showDeck == True then div [] [ h2[] [ text ("Deck") ], div [ style "font-size" "5em" ] [ text deck ] ]
                 else div [] [ h2[] [ text "Deck unvisible" ] ] ]
      , div [] [ ]
      ]
