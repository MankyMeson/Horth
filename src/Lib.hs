module Lib where

import HorthVal
import Lexer
import GHC.Float
import Data.Maybe


digits :: String
digits = "-0123456789"

fltDigits :: String
fltDigits = '.':digits

-- "Covert" words to be ignored by any function applied to the stack
horthWords :: [HorthVal]
horthWords = map Word ["if", "then", "else", "elif", "while", "for", "endif"]

-- operations and the number of arguments they require in the stack (N.B. "cp" requires as many arguments
-- as the integer it acts upon but this can be implemented later in the evaluation process)
operations :: [String]
operations  =                [".",".s","dup","cp","dump","+","-","/","*","^",">","<",">=","<=","==","&&","||","¬","if","then","else","endif"]
nArgsLookup :: [(String, Int)]
nArgsLookup = zip operations [1  ,0   ,1    ,1   ,1     ,2  ,2  ,2  ,2  ,2  ,2  ,2  ,2   ,2   ,2   ,2   ,2   ,1  ,0   ,0     ,0     ,0      ]

recurseOnly :: Int -> ((a->a)->(a->a)) -> (a->a)
recurseOnly n funK = compose (replicate n funK) id
compose :: [a -> a] -> a -> a
compose = foldr (.) id

newtype HorthStack = HorthStack [HorthVal] deriving (Show)

unStack :: HorthStack -> [HorthVal]
unStack (HorthStack xs) = xs

pushStack :: HorthVal -> HorthStack -> HorthStack
pushStack val stack = HorthStack $ val:unStack stack

dumpStack :: HorthStack -> HorthStack
dumpStack (HorthStack []) = HorthStack []
dumpStack (HorthStack (x:xs))
  | x `elem` horthWords = HorthStack (x:unStack (dumpStack $ HorthStack xs))
  | otherwise           = HorthStack xs

copyElem :: HorthStack -> HorthStack
copyElem (HorthStack ((HorthInt x):xs)) = pushStack (stackElem (HorthStack xs) x) (HorthStack xs)
copyElem x = HorthStack [Nil]

stackElem :: HorthStack -> Int -> HorthVal
stackElem (HorthStack (x:xs)) 0
  | x `elem` horthWords = stackElem (HorthStack xs) 0
  | otherwise           = x
stackElem (HorthStack (x:xs)) n
  | x `elem` horthWords = stackElem (HorthStack xs) n
  | otherwise           = stackElem (HorthStack xs) (n-1)
stackElem (HorthStack []) n = Nil

stackCount :: HorthStack -> Int
stackCount (HorthStack [x])
  | x `elem` horthWords = 0
  | otherwise           = 1
stackCount (HorthStack (x:xs))
  | x `elem` horthWords = stackCount (HorthStack xs)
  | otherwise           = 1 + stackCount (HorthStack xs)
stackCount(HorthStack []) = 0

removeIf :: HorthStack -> HorthStack
removeIf (HorthStack (x:xs))
  | x == Word "if" = HorthStack xs
  | otherwise        = HorthStack (x:unStack (removeIf $ HorthStack xs))
removeIf (HorthStack []) = HorthStack []


readHorthVal :: String -> HorthVal
readHorthVal str
  | isInt str = HorthInt (read str :: Int)
  | isFlt str = HorthFlt (read str :: Double)
  | isStr str = HorthStr (read str :: String)
  | str == "True" || str == "False" = HorthBool (read str :: Bool)
-- Add List functionality here
  | str == "Nil" = Nil
  | otherwise = HorthStr str

isInt :: String -> Bool
isInt (x:xs)
  | x `elem` digits = isInt xs
  | otherwise       = False
isInt x = True

isFlt :: String -> Bool
isFlt [] = False
isFlt (x:xs)
  | x `elem` digits = isFlt xs
  | x == '.'        = isFlt' xs
  | otherwise       = False

isFlt' :: String -> Bool
isFlt' [] = True
isFlt' (x:xs)
  | x `elem` digits = isFlt' xs
  | x == '.'        = False
  | otherwise       = False

isStr :: String -> Bool
isStr xs
  | length xs < 2 = False
  | otherwise     = (head xs == '\"') && (last xs == '\"')

quoteStack :: HorthStack -> String
quoteStack stack = "head -> [" ++ quoteList stack'
  where stack'   = unStack stack

eval :: [String] -> HorthStack -> IO ()
eval (x:xs) stack = do
  let nArgs = lookup x nArgsLookup
  if isNothing nArgs
    then do
      let stack' = pushStack (readHorthVal x) stack
      eval xs stack'
    else if fromJust nArgs > stackCount stack
      then putStrLn "Error: Stack Underflow"
      else do
        case x of
          "."     -> do
            putStr $ quote $ head $ unStack stack
            let stack' = dumpStack stack
            eval xs stack'
          ".s"    -> do
            putStrLn $ quoteStack stack
            eval xs stack
          "dup"   -> do
            let stack' = pushStack (stackElem stack 0) stack
            eval xs stack'
          "cp"    -> do
            let stack' = copyElem stack
            eval xs stack'
          "dump"  -> do
            eval xs (dumpStack stack)
          "+"     -> do
            let sum    = horthSum (stackElem stack 0) (stackElem stack 1)
            let stack' = pushStack sum (dumpStack $ dumpStack stack)
            eval xs stack'
          "-"     -> do
            let sum    = horthMinus (stackElem stack 0) (stackElem stack 1)
            let stack' = pushStack sum (dumpStack $ dumpStack stack)
            eval xs stack'
          "*"     -> do
            let prod = horthProd (stackElem stack 0) (stackElem stack 1)
            let stack' = pushStack prod (dumpStack $ dumpStack stack)
            eval xs stack'
          "/"     -> do
            let div = horthDiv (stackElem stack 0) (stackElem stack 1)
            let stack' = pushStack div (dumpStack $ dumpStack stack)
            eval xs stack'
          "^"     -> do
            let div = horthPow (stackElem stack 0) (stackElem stack 1)
            let stack' = pushStack div (dumpStack $ dumpStack stack)
            eval xs stack'
          "<"     -> do
            let bool = evalLT (stackElem stack 0) (stackElem stack 1)
            let stack' = pushStack bool (dumpStack $ dumpStack stack)
            eval xs stack'
          ">"     -> do
            let bool = evalGT (stackElem stack 0) (stackElem stack 1)
            let stack' = pushStack bool (dumpStack $ dumpStack stack)
            eval xs stack'
          "<="    -> do
            let bool = evalLEQ (stackElem stack 0) (stackElem stack 1)
            let stack' = pushStack bool (dumpStack $ dumpStack stack)
            eval xs stack'
          ">="    -> do
            let bool = evalGEQ (stackElem stack 0) (stackElem stack 1)
            let stack' = pushStack bool (dumpStack $ dumpStack stack)
            eval xs stack'
          "&&"    -> do
            let bool = evalAnd (stackElem stack 0) (stackElem stack 1)
            let stack' = pushStack bool (dumpStack $ dumpStack stack)
            eval xs stack'
          "||"    -> do
            let bool = evalOr (stackElem stack 0) (stackElem stack 1)
            let stack' = pushStack bool (dumpStack $ dumpStack stack)
            eval xs stack'
          "¬"     -> do
            let bool = evalNot (stackElem stack 0)
            let stack' = pushStack bool (dumpStack $ dumpStack stack)
            eval xs stack'
          "=="    -> do
            let bool = evalEq (stackElem stack 0) (stackElem stack 1)
            let stack' = pushStack bool (dumpStack $ dumpStack stack)
            eval xs stack'
          "if"    -> do
            let stack' = pushStack (Word "if") stack
            eval xs stack'
          "then"  -> do -- First check if "if" has been inputted, if not raise parse error
            if Word "if" `elem` unStack stack
            then do -- Check if the most recent element in the stack is a boolean
              let proceed = unBool $ stackElem stack 0
              if isJust proceed
              then do -- Check whether to evaluate the 'then' block or the 'else' block
                let stack' = dumpStack $ removeIf stack
                if fromJust proceed
                then do -- evaluate 'then' block by removing anything between 'else' and 'endif' inclusive 
                  let xs' = removeElseBlock xs False
                  eval xs' stack'
                else do -- evaluate 'else' block similarly removes anything between 'then' and 'else' inclusive
                  let xs' = removeThenBlock xs True
                  eval xs' stack'
              else do
                putStrLn "Parse Error: No Boolean detected prior to 'then'"
            else do
              putStrLn "Parse Error: Expecting 'if' prior to then"
          "else"  -> eval xs stack
          "endif" -> eval xs stack
          x       -> putStrLn "Function is either erroneous or WIP"
eval [] stack = putStr "\n"

horthSum :: HorthVal -> HorthVal -> HorthVal
horthSum x y =
  case x of
    HorthInt x' -> case y of
      HorthInt y' -> HorthInt (x'+y')
      HorthFlt y' -> HorthFlt (int2Double x' + y')
      y           -> Nil
    HorthFlt x' -> case y of
      HorthInt y' -> HorthFlt (x'+ int2Double y')
      HorthFlt y' -> HorthFlt (x'+y')
      y           -> Nil
    x           -> Nil

horthMinus :: HorthVal -> HorthVal -> HorthVal
horthMinus x y =
  case x of
    HorthInt x' -> case y of
      HorthInt y' -> HorthInt (y'-x')
      HorthFlt y' -> HorthFlt (y'- int2Double x')
      y           -> Nil
    HorthFlt x' -> case y of
      HorthInt y' -> HorthFlt (int2Double y' -x')
      HorthFlt y' -> HorthFlt (y'-x')
      y           -> Nil
    x           -> Nil

horthProd :: HorthVal -> HorthVal -> HorthVal
horthProd x y =
  case x of
    HorthInt x' -> case y of
      HorthInt y' -> HorthInt (y'*x')
      HorthFlt y' -> HorthFlt (y'* int2Double x')
      y           -> Nil
    HorthFlt x' -> case y of
      HorthInt y' -> HorthFlt (int2Double y' * x')
      HorthFlt y' -> HorthFlt (y'*x')
      y           -> Nil
    x           -> Nil

horthDiv :: HorthVal -> HorthVal -> HorthVal
horthDiv x y =
  case x of
    HorthInt x' -> case y of
      HorthInt y' -> HorthFlt (int2Double y' / int2Double x')
      HorthFlt y' -> HorthFlt (y'/ int2Double x')
      y           -> Nil
    HorthFlt x' -> case y of
      HorthInt y' -> HorthFlt (int2Double y' / x')
      HorthFlt y' -> HorthFlt (y'/x')
      y           -> Nil
    x           -> Nil

horthPow :: HorthVal -> HorthVal -> HorthVal
horthPow x y =
  case x of
    HorthInt x' -> case y of
      HorthInt y' -> HorthInt (y'^x')
      HorthFlt y' -> HorthFlt (y'**int2Double x')
      y           -> Nil
    HorthFlt x' -> case y of
      HorthInt y' -> HorthFlt (int2Double y'**x')
      HorthFlt y' -> HorthFlt (y'**x')
      y           -> Nil
    x           -> Nil

evalLT :: HorthVal -> HorthVal -> HorthVal
evalLT (HorthInt x) (HorthInt y) = HorthBool (y<x)
evalLT (HorthFlt x) (HorthInt y) = HorthBool (int2Double y<x)
evalLT (HorthInt x) (HorthFlt y) = HorthBool (y<int2Double x)
evalLT (HorthFlt x) (HorthFlt y) = HorthBool (y<x)
evalLT x y = Nil

evalGEQ :: HorthVal -> HorthVal -> HorthVal
evalGEQ x y = evalNot $ evalLT x y

evalGT :: HorthVal -> HorthVal -> HorthVal
evalGT (HorthInt x) (HorthInt y) = HorthBool (y>x)
evalGT (HorthFlt x) (HorthInt y) = HorthBool (int2Double y>x)
evalGT (HorthInt x) (HorthFlt y) = HorthBool (y>int2Double x)
evalGT (HorthFlt x) (HorthFlt y) = HorthBool (y>x)
evalGT x y = Nil

evalLEQ :: HorthVal -> HorthVal -> HorthVal
evalLEQ x y = evalNot $ evalGT x y

evalAnd :: HorthVal -> HorthVal -> HorthVal
evalAnd (HorthBool x) (HorthBool y) = HorthBool (x&&y)
evalAnd x y = Nil

evalOr :: HorthVal -> HorthVal -> HorthVal
evalOr (HorthBool x) (HorthBool y) = HorthBool (x||y)
evalOr x y = Nil

evalNot :: HorthVal -> HorthVal
evalNot (HorthBool x) = HorthBool (not x)
evalNot x = Nil

evalEq :: HorthVal -> HorthVal -> HorthVal
evalEq (HorthInt x) (HorthInt y) = HorthBool (y==x)
evalEq (HorthFlt x) (HorthInt y) = HorthBool (int2Double y==x)
evalEq (HorthInt x) (HorthFlt y) = HorthBool (y==int2Double x)
evalEq (HorthFlt x) (HorthFlt y) = HorthBool (y==x)
evalEq x y = Nil

unBool :: HorthVal -> Maybe Bool
unBool (HorthBool x) = Just x
unBool x = Nothing

removeElseBlock :: [String] -> Bool -> [String]
removeElseBlock (x:xs) False
  | x == "else"  = removeElseBlock xs True
  | otherwise    = x : removeElseBlock xs False
removeElseBlock (x:xs) True
  | x == "endif" = xs
  | otherwise    = removeElseBlock xs True
removeElseBlock [] bool = []

removeThenBlock :: [String] -> Bool -> [String]
removeThenBlock (x:xs) False
  | x == "then"  = removeThenBlock xs True
  | otherwise    = x : removeThenBlock xs False
removeThenBlock (x:xs) True
  | x == "else"  = xs
  | otherwise    = removeThenBlock xs True
removeThenBlock  x y = x
