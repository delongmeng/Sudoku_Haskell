-- define some variables:
boardsize = 9
boxsize = 3
cellvals = "123456789"
blank = (== '.')

sudoku :: Board -> [Board]
--sudoku = filter correct . mcp . prune . choices 
sudoku = extract . search . prune . choices 
--sudoku = search . prune . choices 

extract = map (map (map head))

type Matrix a = [[a]]
type Board    = Matrix Char
type Choices = [Char]

correct :: Board -> Bool 
correct b = all nodups (rows b) &&
            all nodups (cols b) &&
            all nodups (boxs b)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = (notElem x xs) && (nodups xs)

rows :: Matrix a -> Matrix a 
rows = id 

cols :: Matrix a -> Matrix a 
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)
-- or just: cols = transpose

boxs :: Matrix a -> Matrix a 
boxs = map ungroup . ungroup . map cols . group . map group 

group :: [a] -> [[a]]
group = groupBy boxsize

--boxsize :: [a] -> Int
--boxsize = sqrt length

groupBy :: Int -> [a] -> [[a]]
groupBy n [] = []
groupBy n x = (take n x) : groupBy n (drop n x)

ungroup :: [[a]] -> [a]
ungroup = concat 

choices :: Board -> Matrix Choices
choices = map (map choose)
choose e = if blank e then cellvals else [e]

-- matrix cartesian product
mcp :: Matrix [a] -> [Matrix a]
mcp = cp . map cp 

-- cartesian product
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

fixed :: [Choices] -> Choices
fixed = concat . filter single 

single :: [a] -> Bool
single [x] = True
single _ = False

reduce :: [Choices] -> [Choices]
reduce css = map (remove (fixed css)) css

remove :: Choices -> Choices -> Choices
remove fs cs = if single cs then cs else delete fs cs 

delete :: Choices -> Choices -> Choices
delete fs [] = []
delete fs (xs:xss) = if xs `elem` fs then (delete fs xss) else xs:(delete fs xss)

prune :: Matrix Choices -> Matrix Choices 
prune = pruneBy boxs . pruneBy cols . pruneBy rows 

pruneBy :: (Matrix Choices -> Matrix Choices) -> (Matrix Choices -> Matrix Choices)
pruneBy f = f . map reduce . f 

blocked :: Matrix Choices -> Bool 
blocked cm = void cm || not (safe cm)

void :: Matrix Choices -> Bool  
void = any (any null)

safe :: Matrix Choices -> Bool 
safe cm = all (nodups . fixed) (rows cm) &&
          all (nodups . fixed) (cols cm) && 
          all (nodups . fixed) (boxs cm) 

expand :: Matrix Choices -> [Matrix Choices]
expand cm = [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
            where (rows1, row:rows2) = break (any best) cm
                  (row1, cs:row2) = break best row 
                  best cs = (length cs == n)
                  n = minchoice cm 

minchoice :: Matrix Choices -> Int
minchoice = minimum . filter (>1) . concat . map (map length) 

search :: Matrix Choices -> [Matrix Choices]
search cm 
  | blocked cm = [] 
  | all (all single) cm = [cm]
  | otherwise = (concat . map (search . prune) . expand) cm

