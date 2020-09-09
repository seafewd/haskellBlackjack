module Blackjack where

import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

-- Define cards
aceOfHearts :: Card
aceOfHearts = Card Ace Hearts
aceOfSpades :: Card
aceOfSpades = Card Ace Spades
aceOfDiamonds :: Card
aceOfDiamonds = Card Ace Diamonds
threeOfClubs :: Card
threeOfClubs = Card (Numeric 3) Clubs


-- Put cards in a hand
hand :: Hand
hand = (aceOfHearts:aceOfSpades:threeOfClubs:aceOfDiamonds:[])


-- display each card in this hand as list of striding
display :: Hand -> [String]
display [] = []
display (c:cs) = displayCard c : display cs


-- display the card as string
displayCard :: Card -> String
displayCard (Card rank suit) = show rank ++ " of " ++ show suit


-- calculate the value of a card
valueCard :: Card -> Int
valueCard (Card r s) = valueRank (Card r s) -- + valueSuit (Card _ s)


-- calculate the value of a rank
valueRank :: Card -> Int
valueRank (Card (Numeric i) _) = i
valueRank (Card Ace _) = 11        -- TODO check value for ACE
valueRank (Card _ _ ) = 10


-- calculate the value of the cards in this hand
-- go through the list of cards
-- for each card, add value of card to sum, then return sum
value :: Hand -> Int
value [] = 0
value (c:cs) = valueCard c + value cs


-- get number of aces in a hand
numberOfAces :: Hand -> Int
-- filter the list of Cards where x has Rank Ace and get the length of that list
numberOfAces cs = length (filter (\x -> rank x == Ace) cs)

-- check if bust
gameOver :: Hand -> Bool
gameOver cs = value cs > 21


-- check if winner
winner :: Hand -> Hand -> Player
winner cb cg
    | value cg > value cb = Guest
    | otherwise = Bank