-- Haskell 5 Card Draw
-- John Peterson & Sean Warren --

module Main 
    where 

import Data.Char 
import Data.List 
import Data.List.Split
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Random 
import Control.Monad.State 

-- Suit of Card
data Suit = Club | Diamond | Heart | Spade   deriving (Eq, Ord, Show)
-- Value of Card (The One is only for incrementing Value)
data Value = One | Two | Three | Four | Five | Six | Seven
          | Eight | Nine | Ten | Jack | Queen
          | King | Ace  deriving (Eq, Ord, Show, Enum)
          
instance Num Value where
  Two + One = Three
  Three + One = Four
  Four + One = Five
  Five + One = Six
  Six + One = Seven
  Seven + One = Eight
  Eight + One = Nine
  Nine + One = Ten
  Ten+ One = Jack
  Jack + One = Queen
  Queen + One = King
  King + One = Ace
  Ace + One = Two

-- Card itself 
data Card = Card {
     value :: Value,
     suit :: Suit}
     deriving (Show)

-- Better Looking to Print for User     
type UICard = (Suit, Value)

-- Card Equals
instance Eq Card where
	Card a b == Card c d = a == c 
-- Card Order
instance Ord Card where
	Card a b > Card c d = a > c 	
	Card a b < Card c d = a < c 
	
-- Possible Hands
data Hand = HighCard Value
          | PairOf Value
          | TwoPair Value Value Deck
          | ThreeOf Value Deck
          | Straight [Value]
          | Flush Suit
          | FullHouse Value Value Deck
          | FourOf Value Deck
          | StraightFlush Deck
             deriving (Show, Eq, Ord)
          

-- Deck Type for Array of Cards
type Deck = [Card]

--Assemble Deck of Standard 52 Cards
mydeck :: Deck
mydeck = [ Card v s | s <- [Club,Diamond,Heart,Spade], v <- [Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace]]

--Shuffle Deck Function
shufflecards :: Deck -> Deck -> IO Deck
shufflecards [] result = return result
shufflecards s result = do
     pos <- randomRIO (1, length s)
     shufflecards (remove_card s pos) (result ++ [get_card s pos])

--Shuffled Deck
shuffledDeck = shufflecards mydeck []    

-- Non-IO Deck
regShuffledDeck = unsafePerformIO(shuffledDeck)

-- Get Card at Position in a Deck of Cards
get_card :: Deck -> Int -> Card
get_card s pos  = s !! (pos - 1)

-- Remove Card at Position in a Deck of Cards
remove_card :: Deck -> Int -> Deck
remove_card s pos = take (pos - 1) s ++ drop pos s

-- Get First Card Out of Deck of Cards
getFirstCard :: Deck -> Card
getFirstCard (x:xs) = x
      
-- Print Cards Out to User
printCards :: Deck -> IO()
printCards [] = do
      putStr "\n"
printCards x =   
      do {
      putStr "1.) ";
      printDeckSizeOneNicely (take 1(x));
      putStr "2.) ";
      printDeckSizeOneNicely (take 1((drop 1(x))));
      putStr "3.) ";
      printDeckSizeOneNicely (take 1((drop 2(x))));
      putStr "4.) ";
      printDeckSizeOneNicely (take 1((drop 3(x))));
      putStr "5.) ";
      printDeckSizeOneNicely (take 1((drop 4(x))));
      }

-- Print Hidden Cards
printHiddenCards [] = do
      putStr "\n"
printHiddenCards x =   
      do {
      putStrLn "1.) (Hidden)";
      putStrLn "2.) (Hidden)";
      putStrLn "3.) (Hidden)";
      putStrLn "4.) (Hidden)";
      putStrLn "5.) (Hidden)";
      }      

-- Print a New Line, Goto Next Line
printNewLine = do {putStr "\n"}

-- Print size of list
printSize x =
      do 
      printNewLine;
      putStr "Size: ";
      let size = length x;
      print size;

-- Prints Remaining Cards left
printRemainingCards x =
      do 
      printNewLine;
      putStr "The Amount of Cards You Need Now Is: ";
      let size = ((subtract (length x))5);
      print size;

-- Makes Array of String to Array of Int
makeInteger :: [String] -> [Int]
makeInteger = map read 

-- Subtract One from Each Element
minusOne es = [ n - 1 | n <- es]

-- Remove Multiple Cards from a Deck of Cards
removeCards :: Deck -> [Int] -> Deck -> [Deck]
removeCards [] _ result = return result
removeCards _ [] result = return result
removeCards cards (x:xs) result = 
      do
      let modCards = (remove_card cards x);
      let subtractOneFromEach = minusOne xs
      removeCards modCards subtractOneFromEach modCards;

-- Solves List Inside List [[ .. ]]
--sideWork :: [Deck] -> Deck     -- (JP) Tried to make this function more dynamic
sideWork = foldr (++) []
  
-- Print Better Representation of the Card Object
getUICard :: Card -> IO()
getUICard a = 
    do
    let aValue = (value a)
    let aSuit = (suit a)
    let theCard = (aValue, aSuit)
    print theCard
          
listSizeOneToObject (x:xs) = x

-- Prints [Card] or Deck that's size == 1 -> Card
printDeckSizeOneNicely :: Deck -> IO()
printDeckSizeOneNicely x = 
    do
    getUICard (listSizeOneToObject x);

-- prints each value in [Card] or Deck 
printValues :: Deck -> IO()
printValues [] = do putStr ""
printValues (x:xs) = 
    do
    let theValue = (value x)
    print theValue
    printValues xs
    
-- Return List of Values from a Deck of Cards
getValues :: Deck -> [Value] -> [[Value]]
getValues [] r = return r
getValues (x:xs) r = 
    do 
    let theValue = (value x)
    let result = (theValue : r)
    getValues xs result

-- Prints each element (Value) in [Value]
printEachValue :: [Value] -> IO()
printEachValue [] = putStrLn "";
printEachValue (x:xs) = 
    do
    let oneValue = x
    print oneValue
    printEachValue xs
    
-- Needed this for [[Value]] to get [Value] (Only way I could figure out how to fix)
doubleArrayToSingle :: [[Value]] -> [Value]
doubleArrayToSingle (x:xs) = x

-- Checks to see if Cards are a Flush (All the same Suit) [BUG: doesn't work if last element in list is not same suit]
isFlush :: Deck -> Bool
isFlush (x:xs) | (length xs) == 0 = True
               | (length xs) == 1 = (suit x) == (suit (head xs))
               | (length xs) > 1 = (suit x) == (suit (head xs)) && isFlush (tail xs)               

isStraight :: [Value] -> Bool
isStraight (x:xs) | (length xs) == 0 = False
                  | (length xs) == 1 = (x + One) == (head xs)
                  | (length xs) > 1 = (x + One) == (head xs) && isStraight (tail xs)      

isPair :: [Value] -> Bool
isPair (x:xs)     | (length xs) == 0 = True
                  | (length xs) == 1 = x == (head xs)
                  | (length xs) > 1 = x == (head xs) && isPair (tail xs) 				  

               
--isFourOfAKind :: Deck -> Bool
--isFullHouse :: Deck -> Bool
--isThreeOfAKind :: Deck -> Bool
--isTwoPair :: Deck -> Bool

               
--getHighestValueCard :: Deck -> Card
--getHighestValueCard a = maximum a

------- FOR TESTING -------
cardA = Card Two Diamond
cardB = Card Three Diamond
cardC = Card Four Diamond
cardD = Card Five Diamond
cardE = Card Seven Heart
fakeCards = [cardA, cardB, cardC, cardD, cardE]
------- FOR TESTING -------

--isPair :: Deck -> Bool
--isPair (x:xs) | (length xs) > 0 = (value x) == 

-- Processes Cards for Best Hand
processCards :: Deck -> Hand
processCards d | isStraight (sort (doubleArrayToSingle (getValues d [])))
                   && isFlush d = StraightFlush d
               | isStraight (sort (doubleArrayToSingle (getValues d []))) = Straight (sort (doubleArrayToSingle (getValues d [])))
               | isFlush d = Flush (suit (head d))
			   | isPair (sort (doubleArrayToSingle (getValues d []))) = PairOf (value (head d))
               {-
               | isFullHouse d = FullHouse
               | isFourOfAKind d =FourOfAKind
               | isThreeOfAKind d = ThreeOfAKind
               | isTwoPair d = TwoPair
               | isPair d = Pair
               -}
               | otherwise = HighCard (last (sort (doubleArrayToSingle (getValues d []))))

bestHands :: Hand -> Hand -> Hand
bestHands a b | a < b = b
              | a > b = a
              | a == b = a
------------------------------------------------------------------------------------------------    
--Start of Program
main :: IO ()
main = do 
  hSetBuffering stdout NoBuffering 
  putStr "\nWelcome to 5 Card Draw\n"
  putStrLn "It will be you against a CPU player."
  putStrLn "There will be one round of discarding any cards you don't want."
  putStrLn "Afterwards, the best hand wins!\n"
  
  -- Get your cards and drop them from the deck
  let yourCards = take 5(regShuffledDeck)
  let deckAfterUser = drop 5(regShuffledDeck)
  
  putStrLn "Your Cards are: "
  printCards yourCards
    
  printNewLine
  let compOne = take 5(deckAfterUser)
  let deckAfterComp = drop 5(deckAfterUser)
  putStrLn "The Computer Cards are: "
  printHiddenCards compOne --Hidden From User
    
  printNewLine
  putStrLn "Pick cards you'd like to discard for new cards."
  putStrLn "You can select none \"\", some \"1,2,4\", or all \"1,2,3,4,5\""
  putStrLn "Must be in format \"#,#,#\" (No Quotes). Hit enter to finish."
  putStrLn "Which cards would you like to discard?"
  discardCardsInput <- getLine
  let charArray = splitOn "," discardCardsInput --Makes "1,2,3" -> ["1","2","3"]
  let numberArray = makeInteger charArray -- Makes ["1","2","3"] -> [1,2,3]
  
  let mod = (sideWork(removeCards yourCards numberArray [])) -- (SW)created sidework to make the un fold the double list
  --printRemainingCards mod -- throws non-exhaustive exception
    -- (JP) THERES A BUG RIGHT HERE, but this is as far as I was going to go tonight
    -- The problem is that "mod" is a list within a list of cards, notice the [[...]]
    -- Some how have to remove the outer list before we keep working with it
    -- Should be: [Card, Card, Card] not [[Card, Card, Card]] --(SW)fixed this bug

  printNewLine
  putStrLn "Your New Cards are: "
  let yourCardsAfterDiscarding = (take ((subtract (length mod))5) (deckAfterComp)) ++ mod -- makes your deck now 
  printCards yourCardsAfterDiscarding
  
  printNewLine
  putStrLn "The Computer Cards are: "
  printCards compOne
  
  --printNewLine
  --printValues yourCardsAfterDiscarding --I was doing this before just printing the values but couldn't figure out how to store them.

  --printNewLine
  --let justYourValues = (doubleArrayToSingle (getValues yourCardsAfterDiscarding [])) -- Couldn't figure out why I had to have it return [[Value]] but used doubleArrayToSingle to fix it.
  --printEachValue justYourValues
  
  let yourHand = processCards yourCardsAfterDiscarding
  let computersHand = processCards compOne
  
  printNewLine
  putStr "Your Hand: " 
  print yourHand
  putStr "Computer's Hand: "
  print computersHand
  
  --let didUserWin = 
  
  let winningHand =  bestHands yourHand computersHand
  printNewLine
  putStr "Best Hand: "
  print winningHand
  
  printNewLine
  putStrLn "Finished."
