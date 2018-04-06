> module IPFProc where
>
> import Data.Array
> import IPFInputOutput
> import Data.List
>
> import IPFNum 
>
> -- functions for summing up the list of doubles in certain  row or column of an array,
> 
> sumrow :: Num a => (Array (Int,Int) a) -> Int  -> a
> sumrow mx i 
>           | i < li                      = 0
>           | i > ui                      = 0
>           | otherwise                   = sum [mx!(i,j)| j <- range (lj,uj)]
>          where ((li,lj),(ui,uj)) = bounds mx
>
> sumcol :: Num a => (Array (Int,Int) a) -> Int  -> a
> sumcol mx j 
>           | j < lj                      = 0
>           | j > uj                      = 0 
>           | otherwise                   = sum [mx!(i,j)| i <- range (li,ui)]
>          where ((li,lj),(ui,uj)) = bounds mx
>
> -- one step of adapting in IPF, rowwise or columnwise, marginals are Integer
>
> adaptrow :: IPFNum a => [Int] -> (Array (Int,Int) a) -> (Array (Int,Int) a)
> adaptrow  mrow matr = array (bounds matr)
>                              [((i,j), (matr!(i,j)* divide (fromIntegral (mrow !! i)) (sumrow matr i)) ) |
>                                          i <- [li .. ui], j <-[lj .. uj]]
>                       where ((li,lj),(ui,uj)) = bounds matr
>
> adaptcol :: IPFNum a => [Int] -> (Array (Int,Int) a) -> (Array (Int,Int) a)
> adaptcol  mcol matr = array (bounds matr)
>                              [((i,j), (matr!(i,j)* divide (fromIntegral (mcol !! j)) (sumcol matr j)) ) |
>                                         i <- [li .. ui], j <-[lj .. uj]]
>                       where ((li,lj),(ui,uj)) = bounds matr
> 
> {-- one step of adapting in IPF, rowwise or columnwise, marginals are Double,
>     index of mcol/mrow are relative to li,lj
> --}
> 
> adaptrow' :: IPFNum a => [a] -> (Array (Int,Int) a) -> (Array (Int,Int) a)
> adaptrow'  mrow matr = array (bounds matr)
>                              [((i,j), (matr!(i,j)* divide (mrow !! (i-lj)) (sumrow matr i)) ) |
>                                          i <- [li .. ui], j <-[lj .. uj]]
>                       where ((li,lj),(ui,uj)) = bounds matr
>
> adaptcol' :: IPFNum a => [a] -> (Array (Int,Int) a) -> (Array (Int,Int) a)
> adaptcol'  mcol matr = array (bounds matr)
>                              [((i,j), (matr!(i,j)* divide (mcol !! (j-lj)) (sumcol matr j)) ) |
>                                          i <- [li .. ui], j <-[lj .. uj]]
>                       where ((li,lj),(ui,uj)) = bounds matr
>
> ipfRec :: Int -> [Int]-> [Int] -> (Array (Int,Int) Double) -> (Array (Int,Int) Double)
> ipfRec 0  _ _ mx       = mx
> ipfRec n  mrow mcol mx = adaptrow mrow (adaptcol mcol ( ipfRec (n-1) mrow mcol mx))
>
>
>
> --One-Step-IPF for Doubles, input is one array, that is split up in the process to generate
> --mrow and mcol
>
> ipf_one :: IPFNum a => (Array (Int,Int) a) -> (Array (Int,Int) a)
> ipf_one mx =  fuseArr (adaptrow' mrow (adaptcol' mcol mx')) mcol mrow
>                    where mcol = fstLine mx
>                          mrow = fstCol mx
>                          mx' = innerArr mx
>
> ipf_one' :: IPFNum a => (Array (Int,Int) a) -> (Array (Int,Int) a)
> ipf_one' mx =  fuseArr (adaptcol' mcol (adaptrow' mrow mx')) mcol mrow
>                    where mcol = fstLine mx
>                          mrow = fstCol mx
>                          mx' = innerArr mx
> --MultiStep-IPF for Doubles, gets number of rounds as a parameter
> 
> ipf_rec ::  Int -> (Array (Int,Int) Double) -> (Array (Int,Int) Double)
> ipf_rec 1 mx = ipf_one mx 
> ipf_rec n mx = ipf_one (ipf_rec (n-1) mx)
>                
> {- this function does a number of iterations of IPF and saves every step, outputting the list of steps -}
>
> ipfHistory :: IPFNum a => Int -> (Array (Int,Int) a) -> [(Array (Int,Int) a)]
> ipfHistory 0 mx = [mx]
> ipfHistory n mx = mx:(ipfHistory (n-1) (ipf_one mx))
>
>
> {- this function calculates ipfHistory and then transforms all the steps into nice-looking
>    strings and concatanates them with newlines inbetween -}
> 
> ipfString :: IPFNum a => Int -> (Array (Int,Int) a) -> String
> ipfString n = (intercalate "\n").(map printArr).(ipfHistory n)
>
>  
>
> 
> 
> 
> {-- 1. DERIVE INPUT FOR IPF FROM INPUT ARRAY:
>     - the marginal distribution of columns with 'fstLine'
>     - the marginal distribution of rows with 'fstCol'
>     - the microsample (or result from earlier round) with 'innerArr'.
>
>    fstLine: gives the li-th line of an arry of type A as a list of type A starting with entry (li,lj+1) 
>    fstCol:  gives the lj-th column of an arry of type A as a list of type A, starting with entry (li+1,lj)  
>    innerArr: returns an array mx' with bounds (li+1,lj+1)(ui,uj), s.t. mx'(i,j) = mx (i,j) when defined 
> --}
>

> fstLine :: (Array (Int,Int) a) -> [a]
> fstLine mx = [mx ! (li,j)| j <- [(lj+1) .. uj]]
>              where ((li,lj),(ui,uj)) = bounds mx
>            
>
> fstCol :: (Array (Int,Int) a) -> [a]
> fstCol mx = [mx ! (i,lj)| i <- [li+1 .. ui]]
>             where ((li,lj),(ui,uj)) = bounds mx
>
>
>
> innerArr ::  (Array (Int,Int) a) -> (Array (Int,Int) a)
> innerArr mx = array (((li+1),(lj+1)),(ui,uj))
>                         [((i,j), mx !(i,j)) |
>                             i <- [(li+1)..ui], j <- [(lj+1)..uj]]
>              where ((li,lj),(ui,uj)) = bounds mx
>
> {-- 2. PREPARE RESULTS OF IPF FOR REPETITIVE OUTPUT INPUT ARRAY
>      -- fuseArr adds to an arry the given lists, the entry at index (li-1,lj-1) is the sume of list 1
> --}
> 
> fuseArr :: Num a => (Array (Int,Int) a) -> [a] -> [a] -> (Array (Int,Int) a)
> fuseArr mx mcol mrow = array ((li-1,lj-1),(ui,uj))
>                           ([(((li-1),(lj-1)), sum mcol)] ++
>                            [(((li-1),k) , mcol !! (k-lj))| k <- [lj..uj]] ++
>                            (foldl (++) [] [(newrow i mrow mx) | i <- [li..ui]]))                          
>                         where ((li,lj),(ui,uj)) = bounds mx
>                               newrow :: Int -> [a] ->(Array (Int,Int) a) -> [((Int,Int),a)]
>                               newrow i mrow mx = ((i,(lj-1)), mrow !! (i-li)) :
>                                                   [((i,j), (mx ! (i,j))) | j <- range (lj,uj)]
>                                                    where ((li,lj),(ui,uj)) = bounds mx
>
> --                            newrow  adds the ith value from 'mrow' to the (relative) ith line
> --                            of an array 'mx' on the first position
> 
> 
>
> 
> 
>                           
> {-- 3. FOR TESTING PURPOSES
>   -- instead of calculating the results adcol produces the string expressing what should be calculated here,
>   -- adcolC mimics the results of adaptcol
> --}
> 
> adcolC :: Show a => Num a => [a] -> (Array (Int,Int) a) -> (Array (Int,Int) String)
> adcolC  mcol matr = array (bounds matr)
>                              [((i,j), ((show (matr!(i,j))) ++"*" ++ (show (mcol !! j)) ++ "/" ++
>                                        (show (sumcol matr j)))) |
>                                          i <- range (li,ui), j <- range (lj,uj)]
>                       where ((li,lj),(ui,uj)) = bounds matr
> -- is okay
> adrowC :: Show a => Num a => [a] -> (Array (Int,Int) a) -> (Array (Int,Int) String)
> adrowC  mrow matr = array (bounds matr)
>                              [((i,j), ((show (matr!(i,j))) ++"*" ++ (show (mrow !! i)) ++ "/" ++
>                                        (show (sumrow matr i)))) |
>                                          i <- [li .. ui], j <- [lj .. uj]]
>                       where ((li,lj),(ui,uj)) = bounds matr

