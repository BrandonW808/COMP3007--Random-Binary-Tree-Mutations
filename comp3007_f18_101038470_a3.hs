-- Student: Brandon Ward
-- Student #: 101038470

import Codec.BMP
import GHC.Word
import Data.ByteString
import Debug.Trace
import Data.Maybe
import Data.Typeable

--The Tree Structure's Values
data TreeVal = Lit Char | Val Float
  deriving(Eq, Show)

--Recursive Tree Structure
data ArithmeticTree
  = Value TreeVal
  | Add (ArithmeticTree) (ArithmeticTree)
  | Subtract (ArithmeticTree) (ArithmeticTree)
  | Multiply (ArithmeticTree) (ArithmeticTree)
  | Divide (ArithmeticTree) (ArithmeticTree)
  deriving(Eq)

--Function written to address requirement (3i)
evaluateTreeWithFloat :: ArithmeticTree -> Float -> Float
evaluateTreeWithFloat (Add l r) f = (evaluateTreeWithFloat l f) + (evaluateTreeWithFloat r f)
evaluateTreeWithFloat (Subtract l r) f = (evaluateTreeWithFloat l f) - (evaluateTreeWithFloat r f)
evaluateTreeWithFloat (Divide l r) f = (evaluateTreeWithFloat l f) / (evaluateTreeWithFloat r f)
evaluateTreeWithFloat (Multiply l r) f = (evaluateTreeWithFloat l f) * (evaluateTreeWithFloat r f)
evaluateTreeWithFloat (Value (Val v)) _ = v
evaluateTreeWithFloat (Value (Lit c)) f = f

--Function written to address requirement (3ii)
showTree :: ArithmeticTree -> [Char]
showTree (Add l r) = ("(" ++ (showTree l) ++ " + " ++ (showTree r) ++ ")")
showTree (Subtract l r) = ("(" ++ (showTree l) ++ " - " ++ (showTree r) ++ ")")
showTree (Multiply l r) = ("(" ++ (showTree l) ++ " * " ++ (showTree r) ++ ")")
showTree (Divide l r) = ("(" ++ (showTree l) ++ " / " ++ (showTree r) ++ ")")
showTree (Value (Val v)) = (show v)
showTree (Value (Lit c)) = [c]

--Function written to address requirement (3iii)
drawGraphicalTree :: ArithmeticTree -> [[Char]]
drawGraphicalTree (Value (Val v)) = [("--- " ++ (show v))]
drawGraphicalTree (Value (Lit c)) = [("--- " ++ (show c))]
drawGraphicalTree (Add (Value v) r) = [("( + )" ++ ("--- " ++ (show v)))] ++ (drawGraphicalTree r)
drawGraphicalTree (Subtract (Value v) r) = [("( - )" ++ ("--- " ++ (show v)))] ++ (drawGraphicalTree r)
drawGraphicalTree (Multiply (Value v) r) = [("( * )" ++ ("--- " ++ (show v)))] ++ (drawGraphicalTree r)
drawGraphicalTree (Divide (Value v) r) = [("( / )" ++ ("--- " ++ (show v)))] ++ (drawGraphicalTree r)
drawGraphicalTree (Add l r) = ["( + )"] ++ (drawGraphicalTree l) ++ (drawGraphicalTree r)
drawGraphicalTree (Subtract l r) = ["( - )"] ++ (drawGraphicalTree l) ++ (drawGraphicalTree r)
drawGraphicalTree (Multiply l r) = ["( * )"] ++ (drawGraphicalTree l) ++ (drawGraphicalTree r)
drawGraphicalTree (Divide l r) = ["( / )"] ++ (drawGraphicalTree l) ++ (drawGraphicalTree r)

--Help function to drawGraphicalTree to grab a single line
drawGraphicalTreeLine :: ArithmeticTree -> [Char]
drawGraphicalTreeLine (Value (Val v)) = ("--- " ++ (show v))
drawGraphicalTreeLine (Value (Lit c)) = ("--- " ++ (show c))


--Parent function for mutating trees
mutateTree :: ArithmeticTree -> ArithmeticTree
muatateTree (Value (Val v)) = mutateFloatValue v
mutateTree (Add l r) = Add (mutateNode l) (mutateNode r)
mutateTree (Subtract l r) = Subtract (mutateNode l) (mutateNode r)
mutateTree (Multiply l r) = Multiply (mutateNode l) (mutateNode r)
mutateTree (Divide l r) = Divide (mutateNode l) (mutateNode r)

--Function to mutate nodes
mutateNode :: ArithmeticTree -> ArithmeticTree
mutateNode (Value (Val v)) = mutateFloatValue v
mutateNode (Value (Lit c)) = (Value (Lit c))
mutateNode (Add l r) = if (((findCongruentialNumber (getFloatFromTree l))) > 0.5) then Add (mutateNode l) (mutateNode r)
  else mutateTreeNode (Add l r)
mutateNode (Subtract l r) = if (((findCongruentialNumber (getFloatFromTree l))) > 0.5) then Subtract (mutateNode l) (mutateNode r)
  else mutateTreeNode (Subtract l r)
mutateNode (Multiply l r) = if (((findCongruentialNumber (getFloatFromTree l))) > 0.5) then Multiply (mutateNode l) (mutateNode r)
  else mutateTreeNode (Multiply l r)
mutateNode (Divide l r) = if (((findCongruentialNumber (getFloatFromTree l))) > 0.5) then Divide (mutateNode l) (mutateNode r)
  else mutateTreeNode (Divide l r)

--Function to mutate a node determined to be a tree
mutateTreeNode :: ArithmeticTree -> ArithmeticTree
mutateTreeNode (Add l r) = Value (Val (findCongruentialNumber' (getFloatFromTree l)))
mutateTreeNode (Subtract l r) = Value (Val (findCongruentialNumber' (getFloatFromTree l)))
mutateTreeNode (Multiply l r) = Value (Val (findCongruentialNumber' (getFloatFromTree l)))
mutateTreeNode (Divide l r) = Value (Val (findCongruentialNumber' (getFloatFromTree l)))

--Function to mutate a node determined to be a Float
mutateFloatValue :: Float -> ArithmeticTree
mutateFloatValue v =
  if ((findCongruentialNumber v) > 0.7) then Value (Val (findCongruentialNumbers v 3))
  else if ((findCongruentialNumber (v*4)) > 0.75) then (Add (Value (Val(findCongruentialNumber' (v)))) (Value (Val(findCongruentialNumber' (v*4)))))
    else if ((findCongruentialNumber (v*4)) > 0.50) then (Subtract (Value (Val(findCongruentialNumber' (v)))) (Value (Val(findCongruentialNumber' (v*4)))))
      else if ((findCongruentialNumber (v*4)) > 0.25) then (Multiply (Value (Val(findCongruentialNumber' (v)))) (Value (Val(findCongruentialNumber' (v*4)))))
        else (Divide (Value (Val(findCongruentialNumber (v)))) (Value (Val(findCongruentialNumber' (v*4)))))

--Helper function to grab a random Float from a tree
getFloatFromTree :: ArithmeticTree -> Float
getFloatFromTree (Value (Val v)) = v
getFloatFromTree (Value (Lit c)) = 0
getFloatFromTree (Add l r) = (getFloatFromTree l) + (getFloatFromTree r)
getFloatFromTree (Subtract l r) = (getFloatFromTree l) + (getFloatFromTree r)
getFloatFromTree (Multiply l r) = (getFloatFromTree l) + (getFloatFromTree r)
getFloatFromTree (Divide l r) = (getFloatFromTree l) + (getFloatFromTree r)

--Funtion written to multiply Floats between 0 and 1
findCongruentialNumber' :: Float -> Float
findCongruentialNumber' v = ((findCongruentialNumber v)*8)

--Function written to address requirement 1
findCongruentialNumber :: Float -> Float
findCongruentialNumber x = (findMod (toMod (x)) 8)/8

--Modulus function written to help findCongruentialNumber
findMod :: Float -> Float -> Float
findMod x y
  |x > y = findMod (x-y) y
  |y > x = x
  |y == x = 0

--Function to grab the value to mod
toMod :: Float -> Float
toMod x = (1*x + 5)

--Function written to list out a sequence of floats
findCongruentialNumbers :: Float -> Integer -> Float
findCongruentialNumbers x y = trace ("#" ++ show y ++ ": x = " ++ show (findCongruentialNumber x)) $ if (y > 0)
  then findCongruentialNumbers ((findCongruentialNumber x*8)) (y-1)
  else findCongruentialNumber x
