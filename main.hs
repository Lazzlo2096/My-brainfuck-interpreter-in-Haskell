-- BF interpreter

import Data.Char -- ord chr

main = interpretBF (parseStringToBfTerms inputBfProg1) ([],[]) 

inputBfProg1 = ",.++.>,..."
inputBfProg2 = "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.+++++++++++++++++++++++++++++.+++++++..+++.-------------------------------------------------------------------------------.+++++++++++++++++++++++++++++++++++++++++++++++++++++++.++++++++++++++++++++++++.+++.------.--------.-------------------------------------------------------------------.-----------------------."

data BfTerm = 
  Increment |
  Decrement |
  Forward |
  Back |
  Read |
  Print
  deriving(Show, Eq)

parseStringToBfTerms = map parseCharToBfTerm

parseCharToBfTerm c
  | c=='+' = Increment
  | c=='-' = Decrement
  | c=='.' = Print
  | c=='>' = Forward
  | c=='<' = Back
  | c==',' = Read

--data World' a = YYY ([a],[a])
type World = ([Int], [Int]) -- Tape --type synonym
---showWorld (x, y) = map chr (x++y) --lol, impl trait Show for World?

---g  = ([72,101,108],[108,111,33]) :: World
---test_showWorld g = do
---  putStrLn $ showWorld g

interpretBF :: [BfTerm] -> World -> IO ()
interpretBF (x:xs) world = do
  newWorld <- applyBfTermToWorld x world
  interpretBF xs newWorld
interpretBF [] world = return ()

applyBfTermToWorld :: BfTerm -> World -> IO World
applyBfTermToWorld term ([], (x:xs)) = applyBfTermToWorld term ([0], (x:xs))
applyBfTermToWorld term (zs, []) = applyBfTermToWorld term (zs, [0])
applyBfTermToWorld term (zs, (x:xs))
  | term==Increment =	do return (zs, succ x :xs)
  | term==Decrement =	do return (zs, pred x :xs)
  | term==Forward =		do return (zs++[x], xs)
  | term==Back = 		do return (init zs, last zs :x:xs)
  | term==Print =		do putChar $ chr x ; return (zs, (x:xs)) 
  | term==Read  =		do x <- getChar; return (zs, (ord x:xs))
  | otherwise =			   return (zs, (x:xs)) -- do nothing
