import Control.DeepSeq

data Stage = Observer | Transform

evaluate :: Stage -> Int
evaluate _ = 0

evaluate_stages :: [Stage] -> Int
evaluate_stages [] = 0
evaluate_stages a@(x:xs)
   | result == 0 = evaluate_stages xs
   | result > 0  = evaluate_stages xs `deepseq` evaluate_stages a
   | otherwise   = result
   where result = evaluate x
