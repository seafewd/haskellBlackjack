{- Lab 2
   Authors: Alexander Korpas, Andreas Hålén, Robin Karlsson
   Lab group: 42
 -}

module Blackjack where

import Cards
import RunGame

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
bankHand = [c2, c3]

badHand :: Hand
badHand = [c1, c2, c2]


-- create a list of strings containing each card
display :: Hand -> [String]
display h = [displayCard c | c <- h]


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
value [] = 0
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
gameOver h = not (value h < 22)


-- check if winner
winner :: Hand -> Hand -> Player
winner g b 
    | gameOver g = Bank
    | gameOver b = Guest
    | value g <= value g = Bank
    | otherwise = Guest