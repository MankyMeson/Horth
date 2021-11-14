module HorthVal where

data HorthVal = Word String
          | HorthStr String
          | HorthInt Int
          | HorthFlt Double
          | List [HorthVal]
          | HorthBool Bool
          | Nil
          deriving (Eq, Show)

quote :: HorthVal -> String
quote x =
  case x of
    Word y      -> show y
    HorthStr y  -> y
    HorthInt y  -> show y
    HorthFlt y  -> show y
    List y      -> "[" ++ quoteList y 
    HorthBool y -> if y then "True" else "False"
    Nil         -> "Type Error"
                
quoteList :: [HorthVal] -> String
quoteList []     = "]"
quoteList [x]    = quote x ++ "]"
quoteList (x:xs) = quote x ++ "," ++ quoteList xs

