module Golf (
  skips, localMaxima, histogram
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


-- Exercise 2
-- write a function that finds all the local maxima in the input list

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x1:[]) = []
localMaxima (x1:x2:[]) = []
localMaxima (x1:x2:x3:xs)
  | (x2>x1) && (x2>x3) = x2 : localMaxima (x3:xs)
  | otherwise          = localMaxima (x2:x3:xs)


-- Exercise 3
-- write a function that takes a list of integers
-- between [0,9] and outputs a histogram

findMax :: [Integer] -> Integer
findMax [] = 0
findMax (x:[]) = x
findMax (x:y:[])
  | x > y      = x
  | otherwise  = y
findMax (x:y:zs) = findMax ((findMax [x,y]) : zs)

countFreq :: [Integer] -> Integer -> Integer
countFreq [] _ = 0
countFreq (x:xs) y
  | x == y     = 1 + (countFreq xs y)
  | otherwise  = countFreq xs y

histBinsi :: [Integer] -> Integer -> [Integer]
histBinsi xs y
  | y == 10    = []
  | otherwise  = (countFreq xs y) : (histBinsi xs (y+1))

histBins :: [Integer] -> [Integer]
histBins xs = histBinsi xs 0

histDrawRow :: [Integer] -> Integer -> String
histDrawRow [] _ = ""
histDrawRow (x:xs) min
  | x >= min   = "*" ++ (histDrawRow xs min)
  | otherwise  = " " ++ (histDrawRow xs min)

histDrawi :: [Integer] -> Integer -> String
histDrawi _ 0 = "==========\n0123456789"
histDrawi binFreqs minFreq = (histDrawRow binFreqs minFreq) ++ "\n" ++ (histDrawi binFreqs (minFreq-1))

histDraw :: [Integer] -> String
histDraw binFreqs = histDrawi binFreqs (findMax binFreqs)

histogram :: [Integer] -> String
histogram xs = histDraw (histBins xs)
