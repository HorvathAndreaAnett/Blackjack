-----------------------
-- Horvath Andrea - Anett
-- 06.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Card exposing (Card(..), Face(..), Suit(..), cardValue, viewCard, cardToString, deck, cardToUnicode)

import Html exposing (..)
import Html.Attributes exposing (style)

{-
  Replace with your definitions from assignment 1
-}
type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Suit = Clubs | Diamonds | Hearts | Spades
type Card = Card Face Suit

faceToString : Face -> String
faceToString face = case face of
    Ace -> "Ace"
    Two -> "Two"
    Three -> "Three"
    Four -> "Four"
    Five -> "Five"
    Six -> "Six"
    Seven -> "Seven"
    Eight -> "Eight"
    Nine -> "Nine"
    Ten -> "Ten"
    Jack -> "Jack"
    Queen -> "Queen"
    King -> "King"

suitToString : Suit -> String
suitToString suit = case suit of
    Clubs -> "Clubs"
    Diamonds -> "Diamonds"
    Hearts -> "Hearts"
    Spades -> "Spades"

cardToString : Card -> String
cardToString (Card face suit) = faceToString(face) ++ " of " ++ suitToString(suit)

cardValue : Card -> List Int
cardValue (Card face _) =
    case face of
        Ace -> [1, 11]
        Two -> [2]
        Three -> [3]
        Four -> [4]
        Five -> [5]
        Six -> [6]
        Seven -> [7]
        Eight -> [8]
        Nine -> [9]
        _ -> [10]

deck : List Card
deck =
    let
        faceList : List Face
        faceList = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
        suitList : List Suit
        suitList = [Clubs, Diamonds, Hearts, Spades]
    in
        let
            deckHelper : List Suit -> List Card -> List Card
            deckHelper s l=
                case s of
                    [] -> l
                    x::xs -> deckHelper xs (List.append l (List.map (\y -> Card y x) faceList))
        in
            deckHelper suitList []

{-
  Modify this function (if needed) to work with your `Card` definition
-}
cardToUnicode : Card -> String
cardToUnicode  card = --Debug.todo "Uncomment and complete after you added your implementations"
   let
       (Card face suit) = card
   in
       case face of
         Ace -> case suit of
           Spades ->"????"
           Hearts -> "????"
           Clubs ->  "????"
           Diamonds -> "????"
         Two -> case suit of
           Spades ->"????"
           Hearts -> "????"
           Clubs ->  "????"
           Diamonds -> "????"
         Three -> case suit of
           Spades ->"????"
           Hearts -> "????"
           Clubs ->  "????"
           Diamonds ->"????"
         Four -> case suit of
           Spades ->"????"
           Hearts -> "????"
           Clubs ->  "????"
           Diamonds -> "????"
         Five -> case suit of
           Spades ->"????"
           Hearts -> "????"
           Clubs ->  "????"
           Diamonds -> "????"
         Six -> case suit of
           Spades ->"????"
           Hearts -> "????"
           Clubs ->  "????"
           Diamonds -> "????"
         Seven -> case suit of
           Spades ->"????"
           Hearts -> "????"
           Clubs ->  "????"
           Diamonds -> "????"
         Eight -> case suit of
           Spades -> "????"
           Hearts ->  "????"
           Clubs ->   "????"
           Diamonds ->  "????"
         Nine -> case suit of
           Spades -> "????"
           Hearts ->  "????"
           Clubs ->   "????"
           Diamonds ->  "????"
         Ten -> case suit of
           Spades ->"????"
           Hearts -> "????"
           Clubs ->  "????"
           Diamonds -> "????"
         Jack -> case suit of
           Spades ->"????"
           Hearts -> "????"
           Clubs ->  "????"
           Diamonds -> "????"
         Queen -> case suit of
           Spades ->"????"
           Hearts -> "????"
           Clubs ->  "????"
           Diamonds -> "????"
         King -> case suit of
           Spades -> "????"
           Hearts -> "????"
           Clubs ->  "????"
           Diamonds -> "????"


{-
  Modify this function (if needed) to work with your `Card` definition
-}
viewCard : Card -> Html msg
viewCard card = --Debug.todo "Uncomment and complete after you added your implementations"
   let
     --suit = Debug.todo "Obtain the suit from card"
     --face = Debug.todo "Obtain the face from card"
     (Card face suit) = card
     faceName = faceToString face
     suitName = suitToString suit
     suitColor s =
       case s of
         Diamonds -> "red"
         Spades -> "black"
         Hearts -> "red"
         Clubs -> "black"
     unicode = cardToUnicode card
   in
     div [style "display" "inline-block"] [
       div [style "font-size" "10em", style "color" (suitColor suit)] [text unicode],
       div [style "font-size" "0.8em"]  [text (cardToString card)]
     ]
