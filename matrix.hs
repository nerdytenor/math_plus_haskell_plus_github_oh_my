import Array
import Ratio

type Fraction = Ratio Integer
type MIndex = (Integer, Integer)
type Matrix = Array MIndex Fraction


-- todo types for array
square :: Matrix
square = array ((1,1),(2,2)) [((1,1), 1), ((1,2), 2), ((2,1),3),((2,2),4)]

row index arr = array ((1,1),(1,end)) [((1,x), arr ! (index, x)) | x <- [1..end]]
                  where end = snd $ snd $ bounds arr

column index arr = array ((1,1),(end,1)) [((x,1), arr ! (x, index)) | x <- [1..end]]
                  where end = snd $ snd $ bounds arr

row_to_array arr = 
  case (bounds arr) of
    ((1,1),(1,_)) -> [arr ! (1,x) | x <- [1..(snd $ snd $ bounds arr)]]
    _ -> error "not a row"
  
column_to_array arr = 
  case (bounds arr) of
    ((1,1),(_,1)) -> [arr ! (x,1) | x <- [1..(fst $ snd $ bounds arr)]]
    _ -> error "not a row"

dot_product :: Matrix -> Matrix -> Fraction
dot_product row column = sum $  zipWith (*) (row_to_array row) (column_to_array column)

matrix_multiply :: Matrix -> Matrix -> Matrix
matrix_multiply ar1 ar2 = 
  if x == y then answer else error "dimension mismatch"
    where (w, x) = snd $ bounds ar1
          (y, z) = snd $ bounds ar2
          answer = array ((1,1),(w,z)) [((a,b), dot_product (row a ar1) (column b ar2)) | a <- [1..w], b <- [1..z]]

identity :: Integer -> Matrix
identity size = array ((1,1),(size,size)) [((x, y),(if x == y then 1 else 0)) | x <- [1..size], y <- [1..size]]

switch_rows :: Integer -> Integer -> Matrix -> Matrix
switch_rows row_a row_b arr = matrix_transform arr val_for
    where val_for(x,y) = arr ! (new_row x, y)
          new_row r
            | r == row_a = row_b
            | r == row_b = row_a
            | otherwise = r

multiply_row :: Integer -> Fraction -> Matrix -> Matrix
multiply_row row_to_multiply factor arr = matrix_transform arr val_for
  where val_for(x,y) = (arr ! (x,y)) * (multiplier x)
        multiplier x = if x == row_to_multiply then factor else 1

matrix_transform :: Matrix -> (MIndex -> Fraction) -> Matrix
matrix_transform arr fun = 
  array (bounds arr) [((x,y), fun(x,y)) | x <- [1..xlim], y <- [1..ylim]]
    where (xlim,ylim) = snd $ bounds arr