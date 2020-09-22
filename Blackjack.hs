{- Lab 2
   Authors: Alexander Korpas, Andreas Hålén, Robin Karlsson
   Lab group: 42
 -}

module Blackjack where

import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)
import Data.List


-- Example Cards and Hands for testing in bottom
{---------------------------}


implementation = Interface
  {  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iDisplay   = display
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

main :: IO ()
main = runGame implementation

{---------------------------}

-- | get a list of all possible ranks
getListOfRanks :: [Rank]
getListOfRanks = [Numeric x | x <- [2..10]] ++ [Jack,Queen,King,Ace]


-- | get a list of all possible suits
getListOfSuits :: [Suit]
getListOfSuits = [Hearts, Spades, Diamonds, Clubs]


-- get a list of cards containing a full deck (52 cards), by combining all suits with all ranks
fullDeck :: Deck
fullDeck = [ Card x y | x <- getListOfRanks, y <- getListOfSuits]


-- draw one card from the deck and put it in a hand
draw :: Deck -> Hand -> (Deck, Hand)
draw [] h = error "Deck is empty"
draw d h = (tail d, head d:h)


-- draw a card from a deck and give it to the dealer's hand
-- if dealer's hand is < 15, keep going, otherwise return the hand
playBank :: Deck -> Hand
playBank d = playBank' d []


-- auxiliary function for playBank
playBank' :: Deck -> Hand -> Deck
playBank' d h
    | value h > 15 = h
    | otherwise    = playBank' d' h'
        where
            (d', h') = draw d h


-- shuffle a deck
-- return a list of sorted (by ascending random Doubles) cards
shuffle :: [Double] -> Deck -> Deck
shuffle deck randoms = [ fst x | x <- ( sortTuples $ zip randoms deck) ]


-- sort a list of tuples consisting of (Card, Int) based on the value of 2nd element (Double)
-- quicksort: divide & conquer strat
sortTuples :: [(Card, Double)] -> [(Card, Double)]
sortTuples [] = []
sortTuples (t:ts) = (sortTuples lesser) ++ [t] ++ (sortTuples greater)
    where
        lesser = filter( \x -> (snd x) < (snd t) ) ts
        greater = filter( \x -> (snd x) >= (snd t) ) ts


-- create a list of strings containing each card
display :: Hand -> String
display [] = []
display [h] = displayCard h
display (h:hs) = displayCard h ++ ", " ++ display hs 


-- display the card as string
displayCard :: Card -> String
displayCard (Card r s) = show r ++ " of " ++ show s


-- calculate the value of a card
valueCard :: Card -> Int
valueCard (Card r s) = valueRank r


-- calculate the value of a rank
valueRank :: Rank -> Int
valueRank (Numeric n) = n
valueRank Ace = 11
valueRank _ = 10


-- calculate the value of the cards in this hand
value :: Hand -> Int
value h
    | val > 21 = val - (numberOfAces h) * 10
    | otherwise = val
    where val = sum [valueCard c | c <- h]


-- get number of aces in a hand
numberOfAces :: Hand -> Int
-- filter the list of Cards where x has Rank Ace and get the length of that list
numberOfAces cs = length (filter (\x -> rank x == Ace) cs)


-- check if bust
gameOver :: Hand -> Bool
gameOver h = value h > 21


-- check if winner
winner :: Hand -> Hand -> Player
winner g b
    | gameOver g = Bank
    | gameOver b = Guest
    | value g <= value b = Bank
    | otherwise = Guest


-- show recursive steps
sizeSteps :: [Int]
sizeSteps = [   size aHand1,
                size (Card (Numeric 2) Hearts : (Card Jack Spades : [])),
                1 + size (Card Jack Spades:[]),
                1 + 1 + size ([]),
                1 + 1 + 0,
                2
            ]


-- Checks if a given card belongs to a given deck
belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs



{--------------Tests---------------}

-- Test to ensure the deck size doesnt change after shuffle
prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand rand) deck = 
    size deck == size (shuffle rand deck)

-- check that fullDeck has 52 cards
prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52




------- Some cards --------

aCard1 :: Card
aCard1 = Card (Numeric 8) Spades

aCard2 :: Card
aCard2 = Card Jack Hearts

ace_of_spades :: Card
ace_of_spades = Card Ace Spades

c1 :: Card
c1 = Card (Numeric 6) Spades

c2 :: Card
c2 = Card Jack Hearts

c3 :: Card
c3 = Card Ace Hearts

c4 :: Card
c4 = Card Ace Diamonds

c5 :: Card
c5 = Card Ace Spades

c6 :: Card
c6 = Card Ace Clubs


------- Some hands --------
aHand1 :: [Card]
aHand1 = [aCard1, ace_of_spades]

pimpHand :: Hand
pimpHand = [c1,c2,c3,c4,c5,c6]

bankHand :: Hand
bankHand = []

anotherHand :: Hand
anotherHand = [c5, c6]

badHand :: Hand
badHand = [c1, c2, c2]

-- full deck of cards
deck :: Deck
deck = fullDeck