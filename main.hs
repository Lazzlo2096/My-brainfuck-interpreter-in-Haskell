-- BF interpreter
-- main = putStr "Hello!"
-- main = interpretBF (parseBF input) [0] 0

input = "+-++." -- [2]

data BFterm = 
  Add |
  Sub |
  Next |
  Back |
  Read |
  Print
  deriving(Show, Eq)

parseBF a = map parseBFterms a

parseBFterms c
  | c=='+' = Add
  | c=='-' = Sub
  | c=='.' = Print
  | c=='>' = Next
  | c=='<' = Back
  | c==',' = Read
  

myMapElement :: [t] -> Int -> (t -> t) -> [t] --Автовыведено
myMapElement list pointer f = let (x,xs) = myPop pointer list   in  myInsert xs pointer (f x)

myPop index list = (list!!index, take index list ++ drop (index+1) list)
myInsert list pointer new_element = let (ys,zs) = splitAt pointer list   in   ys ++ [new_element] ++ zs

interpretBF :: Num a => [BFterm] -> [a] -> Int -> [a] --Автовыведено
interpretBF (x:xs) memSpace pointer
  | null xs = memSpace -- так он не обрабатывает последний символ
  | x==Add = interpretBF xs ( myMapElement memSpace pointer (+1) ) pointer
  | x==Sub = interpretBF xs ( myMapElement memSpace pointer (\x -> x-1) ) pointer
  | x==Print = interpretBF xs ( myMapElement memSpace pointer id ) pointer  --тут должен делать побочный эффект, кек -- пока просто пропускает
 
interpretBF' :: Enum a => [BFterm] -> [a] -> Int -> [a] -- Ещё раз Автовыведено
interpretBF' (x:xs) memSpace pointer = 
  interpretBF' xs ( myMapElement memSpace pointer (getFunc4Value x) ) ((getFunc4Pointer x) pointer)
interpretBF' [] memSpace pointer = memSpace 

getFunc4Value x
  | x==Add = succ
  | x==Sub = pred
  -- | x==Print = id
  | otherwise = id

getFunc4Pointer x
  | x==Next = succ
  | x==Back = pred
  | otherwise = id
  
-- getFunc4memSpace x
  -- | x==Next = ...
  -- | x==Back = ...
  -- | otherwise = ...
 
testt [] = 5
testt (x:[]) = x
testt (x:xs) = testt xs