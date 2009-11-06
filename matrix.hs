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
row_array r m = map (m !) $ range ((r,1),(r,col_count m))

col_array :: Integer -> Matrix x -> [x]
col_array c m = map (m !) $ range ((1,c),(row_count m,c))

matrix_multiply :: (Num x) => Matrix x -> Matrix x -> Matrix x
matrix_multiply ar1 ar2 = 
  if x == y then answer else error "dimension mismatch"
    where (_,(w, x)) = bounds ar1
          (_,(y, z)) = bounds ar2
          answer = array ((1,1),(w,z)) [((a,b), dot_product (row_array a ar1) (col_array b ar2)) | a <- [1..w], b <- [1..z]]

identity :: (Num x) => Integer -> Matrix x
identity size = array ((1,1),(size,size)) [((x, y),(if x == y then 1 else 0)) | x <- [1..size], y <- [1..size]]

scalar_multiply :: (Num x) => x -> Matrix x -> Matrix x
scalar_multiply scalar  = fmap (scalar *) 

col_count :: Matrix x -> Integer
col_count m = cols
  where (_,(_,cols)) = bounds m

row_count :: Matrix x -> Integer
row_count m = rows
  where (_,(rows,_)) = bounds m

switch_rows :: (Num x) => Integer -> Integer -> Matrix x -> Matrix x
switch_rows row_a row_b m = 
    m // ([((row_a, c), m ! (row_b, c)) | c <- [1..(col_count m)]] ++
            [((row_b, c), m ! (row_a, c)) | c <- [1..(col_count m)]])

multiply_row :: (Num x) => Integer -> x -> Matrix x -> Matrix x
multiply_row row factor m = 
  m // [((row, c), (m ! (row,c)) * factor) | c <- [1..(col_count m)]]


add_row_multiple :: (Num x) => Integer -> Integer -> x -> Matrix x -> Matrix x
add_row_multiple r1 r2 multiple m = -- add a multiple of r2 to r1
  m // [((r1, c), (m ! (r1, c)) + multiple * (m ! (r2,c))) | c <- [1..(col_count m)]]


move_non_zero_to_diagonal :: (Num x) => Integer -> Matrix x-> Maybe (Matrix x)
move_non_zero_to_diagonal diagonal m = move_iter diagonal 
  where move_iter cur_row 
          | cur_row > (row_count m) = Nothing
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
  where end = min (row_count m) (col_count m)

reduced_row_echelon_form :: (Fractional x) => Matrix x -> Matrix x
reduced_row_echelon_form m = foldl (flip subtract_up) ref [rc,(rc-1)..1]
  where rc = row_count m
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

sample_matrix = to_matrix [[2,1,-1,8],[-3,-1,2,-11],[-2,1,2,-3%1]]
invertible_matrix = to_matrix [[2,1,-1],[-3,-1,2],[-2,1,2%1]]


augment_matrix :: Num(x) => Matrix x -> Matrix x
augment_matrix m = array ((1,1),(rc,rc  + (col_count m))) $ old_data ++ id_data
      where old_data = map (\v -> (v, m ! v)) $ range (bounds m) 
            idm = identity rc
            rc = row_count m
            id_data =  map (\(x,y) -> ((x,y+(col_count m)), idm ! (x,y))) $ range (bounds idm) 

-- get a subset of the matrix by selecting columns (left, right) inclusive
select_columns :: (Num m) => (Integer,Integer) ->  Matrix m -> Matrix m
select_columns (left, right) m = array ((1,1),(row_count m, right - left + 1)) vals
  where vals =  move_left $ filter_column (>= left) $ filter_column (<= right) $ assocs m 
        filter_column f = filter (\((_,c),_) -> f c)
        move_left = map (\((row,col),val) -> ((row, col - left + 1),val)) 

invert_matrix :: (Fractional x) => Matrix x -> Maybe (Matrix x)
invert_matrix m = if (has_zero_row reduced_original) then Nothing else Just inverted
  where combined_rref = reduced_row_echelon_form $ augment_matrix m
        inverted = select_columns ((col_count m) + 1, (col_count m) * 2) combined_rref 
        reduced_original = select_columns (1, (col_count m)) combined_rref         
        has_zero_row mm = or $ map (all (== 0)) $ to_array mm
