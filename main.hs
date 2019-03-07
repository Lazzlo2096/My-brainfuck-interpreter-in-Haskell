-- BF interpreter

import Data.Char -- ord chr

--main = interpretBF (parseStringToBfTerms inputBfProg2) ([],[]) []
main = interpretBF' (parseStringToBfTerms inputBfProg2) ([],[]) 

inputBfProg1 = "+-++."
inputBfProg2 = "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.+++++++++++++++++++++++++++++,.+++++++,..+++.-------------------------------------------------------------------------------.+++++++++++++++++++++++++++++++++++++++++++++++++++++++.++++++++++++++++++++++++.+++.------.--------.-------------------------------------------------------------------.-----------------------."
--inputBfProg3 = ",>+++++<.>-.<.>-.<.>-.<.>-.<.>-.<.>-." --see bug

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
type World = ([Int], [Int]) -- type synonym
showWorld (x, y) = map chr (x++y) --lol, impl trait Show for World?

g  = ([72,101,108],[108,111,33]) :: World
test_showWorld g = do
  putStrLn $ showWorld g


-- foldM ?
-- interpretBF BfProgrammWithTerms world
interpretBF :: [BfTerm] -> World -> [IO ()] -> IO [()] -- Type is Автовыведено -- кек, IO [()] - шо это ?
interpretBF (x:xs) world aa =
  let (newWorld, b) = applyBfTermToWorld x world
  in interpretBF xs newWorld (aa++b)
interpretBF [] world aa = sequence aa


--interpretBF' :: [BfTerm] -> World -> IO ()
interpretBF' (x:xs) world = do
  newWorld <- applyBfTermToWorld' x world
  interpretBF' xs newWorld
interpretBF' [] world = return ()

--interpretBF' [] world = IO ()


-- sequence_ - ф-ции с подчёркиванием в конце отбразывают результат

-- apply3TupleToFunc f (x,y,z) = f x y z -- Есть ли в Хаскеле такая стандартная функция? Yes:
  -- curry f x y     = f (x,y)
  -- uncurry f (x,y) = f x y

-- вместо пары pointer и [] можно юзать просто (zs, (x:xs)) где x - будет элемент 'под указателем'
-- подсмотрел это у https://github.com/jrp2014/BF/blob/b676af2e286f02ffae8a58c44bd980b27ff2a6c4/src/BF.hs#L65
-- Гарды applyBfTermToWorld лучше заменить на applyBfTermToWorld Increment ... applyBfTermToWorld Decrement ...  и т.д | мне и так больше нравиться, т.к. тогда мне придёться наповторять кучу уродливых "(zs, (x:xs))". Or it may not repeated?
applyBfTermToWorld :: BfTerm -> World -> (World, [IO ()]) -- Автовыведено
applyBfTermToWorld term ([], (x:xs)) = applyBfTermToWorld term ([0], (x:xs)) -- почему только с этой строкой выводиться варнинг если её переместить после определения applyBfTermToWorld ? --Такой ответ не устраивает( потому что паттерны проверяются в том порядке, в каком написаны)
applyBfTermToWorld term (zs, []) = applyBfTermToWorld term (zs, [0])
applyBfTermToWorld term (zs, (x:xs))
  | term==Increment = ((zs, succ x :xs), [])
  | term==Decrement = ((zs, pred x :xs), [])
  | term==Forward = ((zs++[x], xs), [])
  | term==Back = ((init zs, last zs :x:xs), [])
  | term==Print = ((zs, (x:xs)), [ do putChar $ chr x ])
--  | term==Read  = ((zs, (x:xs)), [ do getChar  ]) -- нужно чтобы изза ленивости приостанавливалось вычелесение слудющего элемента
  -- или можно сдеать так, но как (или откуда?) тогда я буду читать? | term==Print = ((zs, (x:xs)), [x] )
  | otherwise = ((zs, (x:xs)), []) -- do nothing



applyBfTermToWorld' :: BfTerm -> World -> IO World
applyBfTermToWorld' term ([], (x:xs)) = applyBfTermToWorld' term ([0], (x:xs))
applyBfTermToWorld' term (zs, []) = applyBfTermToWorld' term (zs, [0])
applyBfTermToWorld' term (zs, (x:xs))
  | term==Increment = do return (zs, succ x :xs)
  | term==Decrement = do return (zs, pred x :xs)
  | term==Forward = do return (zs++[x], xs)
  | term==Back = do return (init zs, last zs :x:xs)
  | term==Print = do putChar $ chr x ; return (zs, (x:xs)) 
  | term==Read  = do x <- getChar; return (zs, (ord x:xs)) -- нужно чтобы изза ленивости приостанавливалось вычелесение слудющего элемента
  -- или можно сдеать так, но как (или откуда?) тогда я буду читать? | term==Print = ((zs, (x:xs)), [x] )
--  | otherwise = (zs, (x:xs)) -- do nothing

  
