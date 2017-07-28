module Golf (
  skips
  ) where


-- Exercise 1
-- define a function "skips"
-- the first list in the output should tbe the same as the input list
-- the second list in the output should oontain every second element from the input list
-- ..the nth list in the output should contain every nth element from the input list

len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

skipsii :: [a] -> Int -> [a]
skipsii list jump
  | len list < jump  = []
  | otherwise        = do
      let sub = drop (jump-1) list
      head sub : skipsii (drop 1 sub) jump

skipsi :: [a] -> Int -> [[a]]
skipsi list 1 = [list]
skipsi list jump = skipsi list (jump-1) ++ [skipsii list jump]

skips :: [a] -> [[a]]
skips list = skipsi list ((len list))
