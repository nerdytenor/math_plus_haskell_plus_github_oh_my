import Array
import Ratio
import List
import Monad (liftM)

type Fraction = Ratio Integer
type MIndex = (Integer, Integer)
type Matrix = Array MIndex Fraction


to_matrix :: [[Fraction]] -> Matrix
to_matrix arr = array ((1,1),(genericLength arr,genericLength $ head arr)) $ concat vals
  where vals =  map mixin $ zip [1..] $ map (zip [1..]) arr
        mixin = \(row, arr) -> map (\(col,val) -> ((row,col),val)) arr

to_array :: Matrix -> [[Fraction]]
to_array m = map (map (\x -> m ! x)) coord_array
  where 
    (_,(rows,cols)) = bounds m
    indexed_rows = zip [1..] $ genericTake rows $ repeat [1..cols]
    coord_array = map (\(r,arr) -> zip (repeat r) arr) $ indexed_rows

to_int_array :: Matrix -> [[Integer]]
to_int_array m = map (map numerator) $ to_array $ scalar_multiply common_denom m
  where common_denom = (foldl1 lcm $ concatMap (map denominator) $ to_array m) % 1

square :: Matrix
square = to_matrix [[1,2],[3,4]]
lump = to_matrix [[1,2,3],[4,5,6]]

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

scalar_multiply :: Fraction -> Matrix -> Matrix 
scalar_multiply scalar m  = matrix_transform m (\(x,y) -> scalar * (m ! (x,y)))

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

add_row_multiple :: Integer -> Integer -> Fraction -> Matrix -> Matrix
add_row_multiple r1 r2 multiple m = matrix_transform m val_for -- add a multiple of r2 to r1
  where val_for(x,y) 
          | x == r1 = (m ! (x,y)) + (m ! (r2,y)) * multiple
          | otherwise = m ! (x,y)

-- this only cares about the bounds - it would be more accurate to describe it is a matrix constructor
matrix_transform :: Matrix -> (MIndex -> Fraction) -> Matrix
matrix_transform arr fun = 
  array (bounds arr) [((x,y), fun(x,y)) | x <- [1..xlim], y <- [1..ylim]]
    where (xlim,ylim) = snd $ bounds arr


move_non_zero_to_diagonal :: Integer -> Matrix -> Maybe Matrix
move_non_zero_to_diagonal diagonal m = move_iter diagonal 
  where (_,(rows,_)) = bounds m
        move_iter cur_row 
          | cur_row > rows = Nothing
          | m ! (cur_row, diagonal) == 0 = move_iter (cur_row + 1)
          | otherwise =  Just $ switch_rows cur_row diagonal m

-- reduce diagonal and all rows below it - return nothing if can't get non zero lead
reduce_diagonal :: Integer -> Matrix -> Maybe Matrix
reduce_diagonal diagonal orig =  liftM (subtractOut (diagonal + 1)) $ liftM multiplyOut $ move_non_zero_to_diagonal diagonal orig
  where 
    (_,(rows,cols)) = bounds orig
    multiplyOut mm = multiply_row diagonal (1 / (mm ! (diagonal, diagonal))) mm -- multiply the diagonal row such that col val is one
    subtractOut row mm -- subtract out the diagonal row from this row and all others such that our column value is zero
      | row > rows = mm
      | otherwise = subtractOut (row + 1) $ add_row_multiple row diagonal mult mm
      where mult = my_val * (-1)
            my_val = mm ! (row, diagonal)
-- as above, returning original matrix if can't get non zero lead
reduce_d :: Integer -> Matrix -> Matrix
reduce_d i m = 
  case (reduce_diagonal i m) of
    Nothing -> m
    Just n -> n


row_echelon_form :: Matrix -> Matrix
row_echelon_form m = foldl (flip reduce_d) m [1..end]
  where end = min rows cols
        (_,(rows,cols)) = bounds m

reduced_row_echelon_form :: Matrix -> Matrix
reduced_row_echelon_form m = foldl (flip subtract_up) ref [end,(end-1)..1]
  where (_,(end,_)) = bounds m
        ref = row_echelon_form m

-- zero out the entries about the lead for the given diagonal
subtract_up :: Integer -> Matrix -> Matrix
subtract_up row orig = case (lead_column) of
  Nothing -> orig
  Just x -> subiter (row - 1) x orig
  where 
      (_,(_,cols)) = bounds $ orig
      lead_column  = lciter 1 
      lciter x
        | x > cols = Nothing
        | orig ! (row, x) == 1 = Just x
        | otherwise = lciter (x + 1)
      subiter r c mm
        | r == 0 = mm
        | otherwise = subiter (r - 1) c $ add_row_multiple r row (val * (-1)) mm
        where val = mm ! (r, c)

example = to_matrix [[3,-9,12,-9,6,15],[3,-7,8,-5,8,9],[0,3,-6,6,4,-5]]
