-- BF interpreter

import Data.Char -- ord chr

main = interpretBF (parseBF input2) ([],[]) []

input = "+-++."
input2 = "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.+++++++++++++++++++++++++++++.+++++++..+++.-------------------------------------------------------------------------------.+++++++++++++++++++++++++++++++++++++++++++++++++++++++.++++++++++++++++++++++++.+++.------.--------.-------------------------------------------------------------------.-----------------------."

data BFterm = 
  Increment |
  Decrement |
  Forward |
  Back |
  Read |
  Print
  deriving(Show, Eq)

parseBF a = map parseBFterms a

parseBFterms c
  | c=='+' = Increment
  | c=='-' = Decrement
  | c=='.' = Print
  | c=='>' = Forward
  | c=='<' = Back
  | c==',' = Read
  
-- data MemSpace a = YYY ([a],[a]) -- world
showMemSpace (x, y) = map chr (x++y)

interpretBF :: [BFterm] -> ([Int], [Int]) -> [IO ()] -> IO [()] -- Автовыведено -- кек, IO [()] - шо это ?
interpretBF (x:xs) memSpace aa = let (a,b) = change x memSpace in interpretBF xs a (aa++b)
interpretBF [] memSpace aa = sequence aa

-- apply3TupleToFunc f (x,y,z) = f x y z -- Есть ли в Хаскеле такая стандартная функция?

-- вместо пары pointer и [] можно юзать просто (zs, (x:xs)) где x - будет элемент подуказателем
-- подсматрел это у https://github.com/jrp2014/BF/blob/b676af2e286f02ffae8a58c44bd980b27ff2a6c4/src/BF.hs#L65
change term (zs, []) = change term (zs, [0])
change term ([], (x:xs)) = change term ([0], (x:xs)) -- почему только с этой строкой выводиться варнинг если её переместить после определения change ?
change term (zs, (x:xs))
  | term==Increment = (( zs, succ x :xs ), [])
  | term==Decrement = (( zs, pred x :xs ), [])
  | term==Forward = (( zs++[x], xs ), [])
  | term==Back = (( init zs, last zs :x:xs ), [])
  | term==Print = ((zs, (x:xs)), [do putStr [chr x]])
  -- или можно сдеать так, но как (или откуда?) тогда я буду читать? | term==Print = ((zs, (x:xs)), [x] )
  | otherwise = ((zs, (x:xs)), []) -- do nothing


g  = ([72,101,108],[108,111,33]) :: ([Int],[Int])
f g = do
  putStrLn $ showMemSpace g
