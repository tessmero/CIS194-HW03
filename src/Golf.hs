module Golf (
  skips
  ) where


-- Exercise 1
-- the first list in the output should tbe the same as the input list
-- the second list in the output should oontain every second element from the input list
-- ..the nth list in the output should contain every nth element from the input list

skipsi :: [a] -> Int -> [a]
skipsi [] jump = []
skipsi list jump = do
  let sub = drop jump list
  head sub : skipsi sub jump

skips :: [a] -> [[a]]
skips list = [skipsi list 1]
