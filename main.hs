-- BF interpreter

-- main = interpretBF (parseBF input) ([],[])

input = "+-++." -- [2]

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

interpretBF :: (Num a, Enum a) => [BFterm] -> ([a], [a]) -> ([a], [a]) -- Автовыведено
interpretBF (x:xs) memSpace = interpretBF xs $ change x memSpace
interpretBF [] memSpace = memSpace 

-- apply3TupleToFunc f (x,y,z) = f x y z -- Есть ли в Хаскеле такая стандартная функция?

-- вместо пары pointer и [] можно юзать просто ( (xss:xs) , (z:zs)) где z - будет элемент подуказателем
-- подсматрел это у https://github.com/jrp2014/BF/blob/master/src/BF.hs#L65
change term (zs, []) = change term (zs, [0])
change term ([], (x:xs)) = change term ([0], (x:xs)) -- почему только с этой строкой выводиться варнинг если её переместить после определения change ?
change term (zs, (x:xs))
  | term==Increment = ( zs, succ x :xs )
  | term==Decrement = ( zs, pred x :xs )
  | term==Forward = ( zs++[x], xs )
  | term==Back = ( init zs, last zs :x:xs )
  | otherwise = (zs, (x:xs)) -- do nothing