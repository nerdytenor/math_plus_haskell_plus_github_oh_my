import Array
import Ratio
import List
import Monad (liftM)

type MIndex = (Integer, Integer)
type Matrix = Array MIndex

to_matrix :: [[x]] -> Matrix x
to_matrix arr = array ((1,1),(genericLength arr,genericLength $ head arr)) $ concat vals
  where vals =  map mixin $ zip [1..] $ map (zip [1..]) arr
        mixin = \(row, arr) -> map (\(col,val) -> ((row,col),val)) arr

to_array :: Matrix x -> [[x]]
to_array m = map (map (\x -> m ! x)) coord_array
  where 
    (_,(rows,cols)) = bounds m
    indexed_rows = zip [1..] $ genericTake rows $ repeat [1..cols]
    coord_array = map (\(r,arr) -> zip (repeat r) arr) $ indexed_rows

-- multiply by lcm denom
to_int_array :: Matrix (Ratio Integer)-> [[Integer]]
to_int_array m = map (map numerator) $ to_array $ scalar_multiply common_denom m
  where common_denom = (foldl1 lcm $ concatMap (map denominator) $ to_array m) % 1

dot_product :: (Num x) => [x] -> [x] -> x
dot_product row column = sum $ zipWith (*) row column

row_array :: Integer -> Matrix x -> [x]
row_array r m = map (m !) $ range ((r,1),(r,cols))
  where (_,(_,cols)) = bounds m

col_array :: Integer -> Matrix x -> [x]
col_array c m = map (m !) $ range ((1,c),(rows,c))
  where (_,(rows,_)) = bounds m

matrix_multiply :: (Num x) => Matrix x -> Matrix x -> Matrix x
matrix_multiply ar1 ar2 = 
  if x == y then answer else error "dimension mismatch"
    where (_,(w, x)) = bounds ar1
          (_,(y, z)) = bounds ar2
          answer = array ((1,1),(w,z)) [((a,b), dot_product (row_array a ar1) (col_array b ar2)) | a <- [1..w], b <- [1..z]]

identity :: (Num x) => Integer -> Matrix x
identity size = array ((1,1),(size,size)) [((x, y),(if x == y then 1 else 0)) | x <- [1..size], y <- [1..size]]

scalar_multiply :: (Num x) => x -> Matrix x -> Matrix x
scalar_multiply scalar m  = matrix_transform m (\(x,y) cur -> scalar * cur)

switch_rows :: (Num x) => Integer -> Integer -> Matrix x -> Matrix x
switch_rows row_a row_b arr = matrix_transform arr val_for
    where val_for (x,y) cur 
            | x == row_a = arr ! (row_b, y)
            | x == row_b = arr ! (row_a, y)
            | otherwise = cur

multiply_row :: (Num x) => Integer -> x -> Matrix x -> Matrix x
multiply_row row_to_multiply factor arr = matrix_transform arr val_for
  where val_for (x,y) cur = if x == row_to_multiply then cur * factor else cur

add_row_multiple :: (Num x) => Integer -> Integer -> x -> Matrix x -> Matrix x
add_row_multiple r1 r2 multiple m = matrix_transform m val_for -- add a multiple of r2 to r1
  where val_for (x,y) cur
          | x == r1 = cur + (m ! (r2,y)) * multiple
          | otherwise = cur


matrix_transform :: Matrix x-> (MIndex -> x-> x) -> Matrix x
matrix_transform arr fun = 
  array (bounds arr) [((x,y), fun (x,y) (arr ! (x,y))) | x <- [1..xlim], y <- [1..ylim]]
    where (_,(xlim,ylim)) = bounds arr


move_non_zero_to_diagonal :: (Num x) => Integer -> Matrix x-> Maybe (Matrix x)
move_non_zero_to_diagonal diagonal m = move_iter diagonal 
  where (_,(rows,_)) = bounds m
        move_iter cur_row 
          | cur_row > rows = Nothing
          | m ! (cur_row, diagonal) == 0 = move_iter (cur_row + 1)
          | otherwise =  Just $ switch_rows cur_row diagonal m

-- reduce diagonal and all rows below it - return nothing if can't get non zero lead
reduce_diagonal :: (Fractional x) => Integer -> Matrix x -> Maybe (Matrix x)
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
reduce_d :: (Fractional x) => Integer -> Matrix x -> Matrix x
reduce_d i m = 
  case (reduce_diagonal i m) of
    Nothing -> m
    Just n -> n


row_echelon_form :: (Fractional x) => Matrix x -> Matrix x
row_echelon_form m = foldl (flip reduce_d) m [1..end]
  where end = min rows cols
        (_,(rows,cols)) = bounds m

reduced_row_echelon_form :: (Fractional x) => Matrix x -> Matrix x
reduced_row_echelon_form m = foldl (flip subtract_up) ref [end,(end-1)..1]
  where (_,(end,_)) = bounds m
        ref = row_echelon_form m

-- zero out the entries about the lead for the given diagonal
subtract_up :: (Num x) => Integer -> Matrix x -> Matrix x
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

example = to_int_array $ reduced_row_echelon_form $ to_matrix [[2,1,-1,8],[-3,-1,2,-11],[-2,1,2,-3%1]]
